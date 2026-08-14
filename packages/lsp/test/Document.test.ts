import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-effect/compiler/Analysis'
import * as Lexer from '@silk-effect/compiler/Lexer'
import * as ModuleSummary from '@silk-effect/compiler/ModuleSummary'
import * as Parser from '@silk-effect/compiler/Parser'
import * as ProjectAnalysis from '@silk-effect/compiler/ProjectAnalysis'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SourceOrigin from '@silk-effect/compiler/SourceOrigin'
import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as Stdlib from '@silk-effect/compiler/Stdlib'
import * as WorkspaceInventory from '@silk-effect/compiler/WorkspaceInventory'
import * as TextMate from '@silk-effect/language/TextMate'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import { SemanticTokenTypes, SymbolKind } from 'vscode-languageserver-types'
import * as Document from '../src/Document.js'

const encoder = new TextEncoder()

const open = (
  text: string,
): Effect.Effect<{
  readonly document: Document.Document
  readonly snapshot: Analysis.FrontendSnapshot
}> =>
  Effect.gen(function* () {
    const bytes = encoder.encode(text)
    const snapshot = yield* Analysis.ofSource('main', bytes)
    const document = Document.make({
      uri: 'file:///project/main.silk',
      version: 1,
      workspace: 'project:/project/silk.toml',
      module: 'main',
      sourceRoot: '/project',
      bytes,
    })
    return { document, snapshot }
  })

const positionAt = (source: string, offset: number) => {
  const before = source.slice(0, offset)
  const lineStart = before.lastIndexOf('\n') + 1
  return { line: before.split('\n').length - 1, character: offset - lineStart }
}

const positionOf = (source: string, spelling: string, occurrence = 0) => {
  let offset = -1
  for (let index = 0; index <= occurrence; index += 1) offset = source.indexOf(spelling, offset + 1)
  return positionAt(source, offset)
}

it.effect('publishes compiler errors as protocol diagnostics', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> i32 { return missing() }'
    const { document, snapshot } = yield* open(source)
    const diagnostics = Document.diagnostics(document, snapshot, () => undefined)
    assert.strictEqual(diagnostics.length, 1)
    const diagnostic = diagnostics[0]
    assert.strictEqual(diagnostic?.code, 'SEM0004')
    assert.strictEqual(diagnostic?.source, 'silk')
    assert.strictEqual(diagnostic?.range.start.line, 0)
    assert.strictEqual(diagnostic?.range.start.character, source.indexOf('missing'))
  }),
)

it.effect('reports a final expression separately from its missing terminal return', () =>
  Effect.gen(function* () {
    const source = 'pub fn foo() -> i32 {\n          a\n}'
    const { document, snapshot } = yield* open(source)

    assert.deepEqual(
      Document.diagnostics(document, snapshot, () => undefined).map(({ code, message, range }) => ({
        code,
        message,
        range,
      })),
      [
        {
          code: 'SEM0006',
          message: 'Unknown value a',
          range: {
            start: { line: 1, character: 10 },
            end: { line: 1, character: 11 },
          },
        },
        {
          code: 'PAR0004',
          message: 'Expected return statement',
          range: {
            start: { line: 1, character: 11 },
            end: { line: 1, character: 11 },
          },
        },
      ],
    )
  }),
)

it.effect('reports no diagnostics for a valid program', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open('pub fn main() -> i32 { return 42 }')
    assert.deepEqual(
      Document.diagnostics(document, snapshot, () => undefined),
      [],
    )
  }),
)

it.effect('hovers the type of the smallest enclosing expression', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> i32 { return 42 }'
    const { document, snapshot } = yield* open(source)
    const hover = Document.hover(document, snapshot, {
      line: 0,
      character: source.indexOf('42'),
    })
    assert.isDefined(hover)
    assert.deepEqual(hover?.contents, { kind: 'markdown', value: '```silk\ni32\n```' })
  }),
)

it.effect('hovers the exact default float width', () =>
  Effect.gen(function* () {
    const source = 'fn value() -> f64 { return 1.25e2 }'
    const { document, snapshot } = yield* open(source)
    const hover = Document.hover(document, snapshot, positionOf(source, '1.25e2'))
    assert.deepEqual(hover?.contents, { kind: 'markdown', value: '```silk\nf64\n```' })
  }),
)

it.effect('hovers nothing outside typed expressions', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open('pub fn main() -> i32 { return 42 }')
    assert.isUndefined(Document.hover(document, snapshot, { line: 0, character: 0 }))
  }),
)

it.effect('maps hover and definition positions after non-ASCII recovery bytes', () =>
  Effect.gen(function* () {
    const source = `🙂 pub fn main() -> i32 { return 42 }`
    const { document, snapshot } = yield* open(source)
    const position = positionOf(source, 'main')
    const hover = Document.hover(document, snapshot, position)
    const definition = Document.definition(document, snapshot, position, () => undefined)
    assert.include(
      typeof hover?.contents === 'object' && 'value' in hover.contents ? hover.contents.value : '',
      'fn main()',
    )
    assert.deepEqual(definition?.targetSelectionRange.start, position)
  }),
)

it.effect('distinguishes a binding and a source-owned standard-library namespace', () =>
  Effect.gen(function* () {
    const source = `import silk.core as Core
pub fn main() -> i32 {
  let mut allocator = Core.make()
  return 0
}`
    const { document, snapshot } = yield* open(source)
    const hoverText = (spelling: string) => {
      const hover = Document.hover(document, snapshot, positionOf(source, spelling))
      return typeof hover?.contents === 'object' && 'value' in hover.contents
        ? hover.contents.value
        : undefined
    }
    assert.strictEqual(
      hoverText('allocator'),
      '```silk\nlet mut allocator: SystemAllocator\n```',
      JSON.stringify(Analysis.diagnostics(snapshot)),
    )
    assert.strictEqual(hoverText('Core'), '```silk\nimport silk/core as Core\n```')
    assert.strictEqual(hoverText('make'), '```silk\npub fn make() -> SystemAllocator\n```')
  }),
)

it.effect('uses one source-like effect function hover at declarations and references', () =>
  Effect.gen(function* () {
    const source = `effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  return run Effect.catch(store(), recover)
}`
    const { document, snapshot } = yield* open(source)
    const declaration = Document.hover(document, snapshot, positionOf(source, 'recover', 0))
    const reference = Document.hover(document, snapshot, positionOf(source, 'recover', 1))
    assert.deepEqual(declaration?.contents, {
      kind: 'markdown',
      value: '```silk\neffect fn recover(error: OutOfMemory) -> i32\n```',
    })
    assert.deepEqual(reference?.contents, declaration?.contents)
  }),
)

it.effect('appends full declaration documentation to definition and reference hovers', () =>
  Effect.gen(function* () {
    const source = `/// Recovers a problem.
///
/// # Examples
/// \`\`\`silk
/// recover(problem)
/// \`\`\`
effect fn recover(problem: Problem) -> i32 { return problem.code }
pub fn main() -> i32 { return recover(Problem { code: 1 }) }
pub struct Problem { pub code: i32 }
`
    const { document, snapshot } = yield* open(source)
    const definition = Document.hover(document, snapshot, positionOf(source, 'recover', 1))
    const reference = Document.hover(document, snapshot, positionOf(source, 'recover', 2))
    const expected = `\`\`\`silk
effect fn recover(problem: Problem) -> i32
\`\`\`

Recovers a problem.

# Examples

\`\`\`silk
recover(problem)
\`\`\``
    assert.deepEqual(definition?.contents, { kind: 'markdown', value: expected })
    assert.deepEqual(reference?.contents, definition?.contents)
  }),
)

it.effect('distinguishes Effect, catch, and a nominal type argument', () =>
  Effect.gen(function* () {
    const source = `struct Problem {}
effect fn recover(error: Problem) -> i32 { return 0 }
pub fn main() -> i32 {
  let recipe = relay(0)
    |> Effect.catch(recover)
  return run recipe
}`
    const { document, snapshot } = yield* open(source)
    const text = (spelling: string, occurrence = 0) => {
      const hover = Document.hover(document, snapshot, positionOf(source, spelling, occurrence))
      return typeof hover?.contents === 'object' && 'value' in hover.contents
        ? hover.contents.value
        : undefined
    }
    assert.strictEqual(text('Effect'), '```silk\nimport silk/effects as Effect\n```')
    assert.include(text('catch') ?? '', 'pub effect fn catch')
    assert.strictEqual(text('Problem', 1), '```silk\nstruct Problem\n```')
  }),
)

it.effect('returns inferred local type inlay hints in the requested range', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return 0
}`
    const { document, snapshot } = yield* open(source)
    assert.deepEqual(
      Document.inlayHints(document, snapshot, {
        start: { line: 0, character: 0 },
        end: { line: 3, character: 1 },
      }),
      [
        {
          position: { line: 1, character: 19 },
          label: ': SystemAllocator',
          kind: 1,
          paddingLeft: false,
          paddingRight: false,
        },
      ],
    )
  }),
)

it.effect('clips inferred hints, skips unavailable bindings, and maps Unicode snapshots', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> i32 {
  let broken = missing()
  // π🙂
  let mut allocator = SystemAllocator.make()
  return 0
}`
    const { document, snapshot } = yield* open(source)
    const range = {
      start: { line: 3, character: 0 },
      end: { line: 3, character: 50 },
    }
    const first = Document.inlayHints(document, snapshot, range)
    assert.deepEqual(Document.inlayHints(document, snapshot, range), first)
    assert.deepEqual(first, [
      {
        position: { line: 3, character: 19 },
        label: ': SystemAllocator',
        kind: 1,
        paddingLeft: false,
        paddingRight: false,
      },
    ])
    assert.deepEqual(
      Document.inlayHints(document, snapshot, {
        start: { line: 0, character: 0 },
        end: { line: 2, character: 0 },
      }),
      [],
    )
  }),
)

it.effect('completes standard-library namespace and source service operations', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> i32 {
  return Effect.
}`
    const { document, snapshot } = yield* open(source)
    const completion = Document.completion(document, snapshot, {
      line: 1,
      character: '  return Effect.'.length,
    })
    assert.include(
      completion.items.map((item) => item.label),
      'catch',
    )
    assert.include(
      completion.items.find((item) => item.label === 'catch')?.detail ?? '',
      'pub effect fn catch',
    )

    const allocatorSource = `pub fn main() -> i32 {
  return Allocator.
}`
    const allocator = yield* open(allocatorSource)
    const allocatorCompletion = Document.completion(allocator.document, allocator.snapshot, {
      line: 1,
      character: '  return Allocator.'.length,
    })
    assert.include(
      allocatorCompletion.items.map((item) => item.label),
      'allocate',
    )
    assert.include(
      allocatorCompletion.items.find((item) => item.label === 'allocate')?.detail ?? '',
      'effect fn allocate',
    )
  }),
)

it.effect('completes visible values through a partial identifier replacement', () =>
  Effect.gen(function* () {
    const source = `fn recover(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  return rec
}`
    const { document, snapshot } = yield* open(source)
    const completion = Document.completion(document, snapshot, {
      line: 2,
      character: '  return rec'.length,
    })
    const recover = completion.items.find((item) => item.label === 'recover')
    assert.isDefined(recover)
    assert.deepEqual(recover?.textEdit, {
      range: {
        start: { line: 2, character: '  return '.length },
        end: { line: 2, character: '  return rec'.length },
      },
      newText: 'recover',
    })
  }),
)

it.effect('completes fields from an inferred local subject type', () =>
  Effect.gen(function* () {
    const source = `struct Pair { left: i32 }
pub fn main() -> i32 {
  let pair = Pair { left: 1 }
  return pair.
}`
    const { document, snapshot } = yield* open(source)
    const completion = Document.completion(document, snapshot, {
      line: 3,
      character: '  return pair.'.length,
    })
    assert.include(
      completion.items.map((item) => item.label),
      'left',
    )
  }),
)

it.effect('uses semantic qualifier lookup and lets a local shadow an intrinsic actor', () =>
  Effect.gen(function* () {
    const source = `struct Pair { left: i32 }
pub fn main() -> i32 {
  let Effect = Pair { left: 1 }
  return Effect.
}`
    const { document, snapshot } = yield* open(source)
    const completion = Document.completion(document, snapshot, {
      line: 3,
      character: '  return Effect.'.length,
    })
    assert.include(
      completion.items.map((item) => item.label),
      'left',
    )
    assert.notInclude(
      completion.items.map((item) => item.label),
      'catch',
    )
  }),
)

it.effect('completes types in damaged parameter and generic-argument positions', () =>
  Effect.gen(function* () {
    const parameterSource = `struct Problem {}
fn identity<T>(value: ) -> i32 { return 0 }`
    const parameter = yield* open(parameterSource)
    const parameterCompletion = Document.completion(parameter.document, parameter.snapshot, {
      line: 1,
      character: 'fn identity<T>(value: '.length,
    })
    assert.include(
      parameterCompletion.items.map((item) => item.label),
      'Problem',
    )
    assert.notInclude(
      parameterCompletion.items.map((item) => item.label),
      'true',
    )

    const argumentSource = `struct Problem {}
pub fn main() -> i32 { return Effect.catch< }`
    const argument = yield* open(argumentSource)
    const argumentCompletion = Document.completion(argument.document, argument.snapshot, {
      line: 1,
      character: 'pub fn main() -> i32 { return Effect.catch<'.length,
    })
    assert.include(
      argumentCompletion.items.map((item) => item.label),
      'Problem',
    )
    assert.notInclude(
      argumentCompletion.items.map((item) => item.label),
      'false',
    )
  }),
)

it.effect('lists constants, functions, and structs with fields as document symbols', () =>
  Effect.gen(function* () {
    const source = `pub const defaultAnswer: i32 = 42
pub struct Box { answer: i32 }
pub fn main() -> i32 { return 42 }`
    const { document, snapshot } = yield* open(source)
    const symbols = Document.symbols(document, snapshot)
    assert.deepEqual(
      symbols.map((symbol) => [symbol.name, symbol.kind]),
      [
        ['defaultAnswer', SymbolKind.Constant],
        ['Box', SymbolKind.Struct],
        ['main', SymbolKind.Function],
      ],
    )
    assert.deepEqual(
      symbols[1]?.children?.map((child) => [child.name, child.kind]),
      [['answer', SymbolKind.Field]],
    )
  }),
)

it.effect('formats a non-canonical document with one whole-document edit', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open('pub fn main() -> i32 { return   42 }')
    const edits = yield* Document.format(document, snapshot)
    assert.strictEqual(edits.length, 1)
    assert.include(edits[0]?.newText, 'return 42')
  }),
)

it.effect('formats a damaged document with no edits', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open('pub fn main( -> {')
    assert.deepEqual(yield* Document.format(document, snapshot), [])
  }),
)

it.effect('converts local, parameter, callable, and field targets into definition links', () =>
  Effect.gen(function* () {
    const source = `struct Pair { left: i32 }
fn identity(value: i32) -> i32 {
  let pair = Pair { left: value }
  return pair.left
}
pub fn main() -> i32 { return identity(42) }`
    const { document, snapshot } = yield* open(source)
    const definitionAt = (spelling: string, occurrence: number) => {
      let character = -1
      for (let index = 0; index <= occurrence; index += 1)
        character = source.indexOf(spelling, character + 1)
      const before = source.slice(0, character)
      const line = before.split('\n').length - 1
      const lineStart = before.lastIndexOf('\n') + 1
      return Document.definition(
        document,
        snapshot,
        { line, character: character - lineStart },
        () => undefined,
      )
    }

    assert.strictEqual(definitionAt('value', 1)?.targetSelectionRange.start.line, 1)
    assert.strictEqual(definitionAt('pair', 1)?.targetSelectionRange.start.line, 2)
    assert.strictEqual(definitionAt('left', 2)?.targetSelectionRange.start.line, 0)
    assert.strictEqual(definitionAt('identity', 1)?.targetSelectionRange.start.line, 1)
  }),
)

it.effect('navigates declaration names and nominal/generic types but not intrinsics', () =>
  Effect.gen(function* () {
    const source = `struct Problem {}
fn recover<T>(error: Problem, value: T) -> T { return value }
pub fn main() -> i32 {
  let allocator = SystemAllocator.make()
  return recover<i32>(0, 0)
}`
    const { document, snapshot } = yield* open(source)
    const definitionAt = (spelling: string, occurrence = 0) =>
      Document.definition(
        document,
        snapshot,
        positionOf(source, spelling, occurrence),
        () => undefined,
      )

    assert.deepEqual(definitionAt('Problem')?.targetSelectionRange.start, {
      line: 0,
      character: 7,
    })
    assert.deepEqual(definitionAt('Problem', 1)?.targetSelectionRange.start, {
      line: 0,
      character: 7,
    })
    assert.deepEqual(definitionAt('T', 1)?.targetSelectionRange.start, {
      line: 1,
      character: 11,
    })
    assert.deepEqual(definitionAt('recover', 1)?.targetSelectionRange.start, {
      line: 1,
      character: 3,
    })
    assert.isUndefined(definitionAt('SystemAllocator'))
    assert.isUndefined(definitionAt('make'))
    assert.isUndefined(definitionAt('i32', 1))
  }),
)

it.effect('uses exact cross-module snapshot sources for qualified definition links', () =>
  Effect.gen(function* () {
    const root = 'import lib\npub fn main() -> i32 { return lib.answer() }'
    const lib = 'pub fn answer() -> i32 { return 42 }'
    const snapshot = yield* Analysis.make({
      root: SourceFile.make('root', encoder.encode(root)),
    }).pipe(Effect.provide(SourceResolver.memory(new Map([['lib', encoder.encode(lib)]]))))
    const document = Document.make({
      uri: 'file:///project/root.silk',
      version: 1,
      workspace: 'project:/project/silk.toml',
      module: 'root',
      sourceRoot: '/project',
      bytes: encoder.encode(root),
    })
    const link = Document.definition(
      document,
      snapshot,
      { line: 1, character: root.slice(root.indexOf('\n') + 1).indexOf('answer') },
      (module) => (module === 'lib' ? 'file:///project/lib.silk' : undefined),
    )
    assert.strictEqual(link?.targetUri, 'file:///project/lib.silk')
    assert.deepEqual(link?.targetSelectionRange, {
      start: { line: 0, character: 7 },
      end: { line: 0, character: 13 },
    })
    const qualifier = Document.definition(
      document,
      snapshot,
      positionOf(root, 'lib', 1),
      () => undefined,
    )
    assert.deepEqual(qualifier?.targetSelectionRange, {
      start: { line: 0, character: 7 },
      end: { line: 0, character: 10 },
    })
  }),
)

it.effect('returns no definition for unavailable targets and trivia', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> i32 { return missing() }'
    const { document, snapshot } = yield* open(source)
    assert.isUndefined(
      Document.definition(
        document,
        snapshot,
        { line: 0, character: source.indexOf('missing') },
        () => undefined,
      ),
    )
    assert.isUndefined(
      Document.definition(document, snapshot, { line: 0, character: 3 }, () => undefined),
    )
  }),
)

interface ProjectModule {
  readonly module: string
  readonly text: string
}

/**
 * Mirrors `Workspace.uriOf`: a toolchain-shipped module answers with its real installed
 * `file://` href, so an edit aimed at the standard library is visible as such in a test.
 */
const uriOfModule = (module: string): string =>
  Stdlib.find(module)?.sourceUrl.href ?? `file:///project/${module}.silk`

/**
 * Mirrors the installed toolchain resolver: project modules come from memory while reserved
 * standard-library modules carry the `ToolchainFile` origin a real installation reports.
 */
const toolchainResolver = (
  sources: ReadonlyMap<string, Uint8Array>,
): Layer.Layer<SourceResolver.SourceResolver> =>
  Layer.succeed(SourceResolver.SourceResolver, {
    resolve: (module) => {
      const bytes = sources.get(module)
      return Effect.succeed(
        bytes === undefined
          ? Option.none()
          : Option.some(SourceResolver.resolved(bytes, SourceOrigin.memory())),
      )
    },
    resolveStandardLibrary: (module) => {
      const entry = Stdlib.find(module)
      return Effect.succeed(
        entry === undefined
          ? Option.none()
          : Option.some(
              SourceResolver.resolved(
                entry.bytes,
                SourceOrigin.toolchainFile(entry.sourceUrl.href),
              ),
            ),
      )
    },
  })

/** Opens several modules as one analyzed project and focuses one of them as the request document. */
const openProject = (
  modules: ReadonlyArray<ProjectModule>,
  focus: string,
): Effect.Effect<{
  readonly document: Document.Document
  readonly snapshot: Analysis.FrontendSnapshot
}> =>
  Effect.gen(function* () {
    const bytes = new Map(
      modules.map(({ module, text }) => [module, encoder.encode(text)] as const),
    )
    const project = yield* ProjectAnalysis.make(
      modules.map(({ module }) => SourceFile.make(module, bytes.get(module) ?? new Uint8Array())),
    ).pipe(Effect.provide(toolchainResolver(bytes)))
    const snapshot = ProjectAnalysis.view(project, focus)
    if (snapshot === undefined) throw new Error(`Project analysis lost focused root ${focus}`)
    return {
      document: Document.make({
        uri: uriOfModule(focus),
        version: 1,
        workspace: 'project:/project/silk.toml',
        module: focus,
        sourceRoot: '/project',
        bytes: bytes.get(focus) ?? new Uint8Array(),
      }),
      snapshot,
    }
  })

const geometry = 'pub fn area(width: i32, height: i32) -> i32 { return width }'
const consumer =
  'import geometry\npub fn main() -> i32 { return geometry.area(2, 3) + geometry.area(4, 5) }'

it.effect('finds every use of one declaration across two modules', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'main', text: consumer },
      ],
      'geometry',
    )
    const position = positionOf(geometry, 'area')
    assert.deepEqual(
      Document.references(document, snapshot, position, true, uriOfModule)?.map(
        ({ uri, range }) => ({ uri, line: range.start.line, character: range.start.character }),
      ),
      [
        { uri: uriOfModule('geometry'), line: 0, character: geometry.indexOf('area') },
        { uri: uriOfModule('main'), line: 1, character: consumer.split('\n')[1]?.indexOf('area') },
        {
          uri: uriOfModule('main'),
          line: 1,
          character: consumer.split('\n')[1]?.indexOf('area', 40),
        },
      ],
    )
    // Excluding the declaration drops exactly the declaration-site occurrence.
    assert.deepEqual(
      Document.references(document, snapshot, position, false, uriOfModule)?.map(({ uri }) => uri),
      [uriOfModule('main'), uriOfModule('main')],
    )
  }),
)

it.effect('renames a declaration with one workspace edit covering both modules', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'main', text: consumer },
      ],
      'geometry',
    )
    const renamed = Document.rename(
      document,
      snapshot,
      positionOf(geometry, 'area'),
      'surface',
      uriOfModule,
    )
    assert.strictEqual(renamed?._tag, 'RenameEdit')
    if (renamed?._tag !== 'RenameEdit') return
    const changes = renamed.edit.changes ?? {}
    assert.deepEqual(Object.keys(changes).sort(), [uriOfModule('geometry'), uriOfModule('main')])
    assert.deepEqual(
      changes[uriOfModule('geometry')]?.map((edit) => edit.newText),
      ['surface'],
    )
    assert.deepEqual(
      changes[uriOfModule('main')]?.map((edit) => edit.newText),
      ['surface', 'surface'],
    )
  }),
)

it.effect('refuses a rename that collides with a top-level name in any edited module', () =>
  Effect.gen(function* () {
    const local = yield* openProject(
      [{ module: 'geometry', text: `${geometry}\npub fn surface() -> i32 { return 1 }` }],
      'geometry',
    )
    const refusedLocally = Document.rename(
      local.document,
      local.snapshot,
      positionOf(geometry, 'area'),
      'surface',
      uriOfModule,
    )
    assert.deepEqual(refusedLocally, {
      _tag: 'RenameRefusal',
      code: 'SEM0016',
      message: 'Multiple bindings claim surface',
    })

    // The importing module's flat namespace counts too: its selected member would collide there.
    const importer = 'import geometry { area }\npub fn surface() -> i32 { return area(1, 2) }'
    const across = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'main', text: importer },
      ],
      'geometry',
    )
    assert.deepEqual(
      Document.rename(
        across.document,
        across.snapshot,
        positionOf(geometry, 'area'),
        'surface',
        uriOfModule,
      ),
      { _tag: 'RenameRefusal', code: 'SEM0016', message: 'Multiple bindings claim surface' },
    )
  }),
)

it.effect('prepares a rename on a name token but fails on a keyword', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [{ module: 'geometry', text: geometry }],
      'geometry',
    )
    assert.deepEqual(Document.prepareRename(document, snapshot, positionOf(geometry, 'area')), {
      range: {
        start: { line: 0, character: geometry.indexOf('area') },
        end: { line: 0, character: geometry.indexOf('area') + 'area'.length },
      },
      placeholder: 'area',
    })
    assert.isUndefined(
      Document.prepareRename(document, snapshot, positionOf(geometry, 'pub')),
      'a keyword token has no renameable declaration',
    )
    assert.isUndefined(Document.prepareRename(document, snapshot, positionOf(geometry, 'return')))
  }),
)

it.effect('renames the imported source name without disturbing a local alias', () =>
  Effect.gen(function* () {
    const aliased =
      'import geometry { area as region }\npub fn main() -> i32 { return region(1, 2) }'
    const { document, snapshot } = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'main', text: aliased },
      ],
      'geometry',
    )
    const renamed = Document.rename(
      document,
      snapshot,
      positionOf(geometry, 'area'),
      'surface',
      uriOfModule,
    )
    assert.strictEqual(renamed?._tag, 'RenameEdit')
    if (renamed?._tag !== 'RenameEdit') return
    const changes = renamed.edit.changes ?? {}
    // Only the `area` half of `area as region` moves; `region` and its uses keep their spelling.
    assert.deepEqual(
      changes[uriOfModule('main')]?.map((edit) => ({
        line: edit.range.start.line,
        character: edit.range.start.character,
        newText: edit.newText,
      })),
      [{ line: 0, character: aliased.indexOf('area'), newText: 'surface' }],
    )
  }),
)

const toolchainUri = Stdlib.find('silk/bool')?.sourceUrl.href
const boolConsumer =
  'import silk.bool { equals }\npub fn main() -> bool { return equals(true, true) }'

it.effect('refuses to rename a declaration the installed toolchain owns', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [{ module: 'main', text: boolConsumer }],
      'main',
    )
    assert.isDefined(toolchainUri)
    // The import binding and the call site both reach the same toolchain-owned declaration.
    for (const occurrence of [0, 1]) {
      const position = positionOf(boolConsumer, 'equals', occurrence)
      const renamed = Document.rename(document, snapshot, position, 'sameAs', uriOfModule)
      assert.strictEqual(renamed?._tag, 'RenameRefusal')
      if (renamed?._tag !== 'RenameRefusal') return
      assert.strictEqual(renamed.code, 'LSP0002')
      assert.include(renamed.message, 'silk/bool')
      // The editor is never offered the rename UI for a toolchain-owned declaration either.
      assert.isUndefined(Document.prepareRename(document, snapshot, position))
    }
  }),
)

const boolAlias =
  'import silk.bool { equals as eq }\npub fn main() -> bool { return eq(true, true) }'
// `eq` never spells `equals`, so these two offsets are the whole extent of the alias.
const aliasBinding = boolAlias.indexOf(' as eq') + ' as '.length
const aliasCall = boolAlias.indexOf('eq(')

it.effect('renames a local alias of a toolchain import, which the project owns', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject([{ module: 'main', text: boolAlias }], 'main')
    assert.isDefined(toolchainUri)
    // The alias is renameable from its binding and from its use: both name `main`'s own binding.
    for (const offset of [aliasBinding, aliasCall]) {
      const position = positionAt(boolAlias, offset)
      const renamed = Document.rename(document, snapshot, position, 'same', uriOfModule)
      assert.strictEqual(renamed?._tag, 'RenameEdit')
      if (renamed?._tag !== 'RenameEdit') return
      const changes = renamed.edit.changes ?? {}
      // Only the project module is edited: the `equals` half of the clause keeps its spelling.
      assert.deepEqual(Object.keys(changes), [uriOfModule('main')])
      assert.deepEqual(
        changes[uriOfModule('main')]?.map((edit) => ({
          ...edit.range.start,
          newText: edit.newText,
        })),
        [
          { ...positionAt(boolAlias, aliasBinding), newText: 'same' },
          { ...positionAt(boolAlias, aliasCall), newText: 'same' },
        ],
      )
      // Prepare must never grey out F2 where the rename itself succeeds.
      assert.deepEqual(Document.prepareRename(document, snapshot, position), {
        range: {
          start: position,
          end: { line: position.line, character: position.character + 'eq'.length },
        },
        placeholder: 'eq',
      })
    }
  }),
)

it.effect('refuses a toolchain-owned rename before any flat-namespace collision', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [{ module: 'main', text: boolConsumer }],
      'main',
    )
    // `main` already occupies this module's flat namespace, so both refusals apply at once and
    // the toolchain one must win: no new spelling can make this rename legal.
    const both = Document.rename(
      document,
      snapshot,
      positionOf(boolConsumer, 'equals'),
      'main',
      uriOfModule,
    )
    assert.strictEqual(both?._tag, 'RenameRefusal')
    if (both?._tag !== 'RenameRefusal') return
    assert.strictEqual(both.code, 'LSP0002')
    // A collision with no toolchain edit behind it still reports the binding conflict.
    assert.deepEqual(
      Document.rename(document, snapshot, positionOf(boolConsumer, 'main'), 'equals', uriOfModule),
      { _tag: 'RenameRefusal', code: 'SEM0016', message: 'Multiple bindings claim equals' },
    )
  }),
)

const boolHelper =
  'import silk.bool { equals }\npub fn same(left: bool, right: bool) -> bool { return equals(left, right) }'
const boolCaller = 'import helper { same }\npub fn main() -> bool { return same(true, true) }'

it.effect('renames a project declaration used beside toolchain imports without touching them', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [
        { module: 'helper', text: boolHelper },
        { module: 'main', text: boolCaller },
      ],
      'helper',
    )
    const renamed = Document.rename(
      document,
      snapshot,
      positionOf(boolHelper, 'same'),
      'identical',
      uriOfModule,
    )
    assert.strictEqual(renamed?._tag, 'RenameEdit')
    if (renamed?._tag !== 'RenameEdit') return
    const changes = renamed.edit.changes ?? {}
    assert.deepEqual(Object.keys(changes).sort(), [uriOfModule('helper'), uriOfModule('main')])
    // No edit may land in the toolchain installation, whatever the project's closure contains.
    assert.isEmpty(
      Object.keys(changes).filter((uri) => uri === toolchainUri || uri.includes('/stdlib/')),
    )
  }),
)

it.effect('still lists the toolchain declaration among a standard-library name references', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [{ module: 'main', text: boolConsumer }],
      'main',
    )
    const locations = Document.references(
      document,
      snapshot,
      positionOf(boolConsumer, 'equals'),
      true,
      uriOfModule,
    )
    assert.isDefined(locations)
    // References stay read-only and complete: the toolchain declaration is useful to show.
    assert.include(
      locations?.map(({ uri }) => uri),
      toolchainUri,
    )
    assert.include(
      locations?.map(({ uri }) => uri),
      uriOfModule('main'),
    )
  }),
)

const stdlibAliases =
  'import silk.bytes { make as bytesMake }\nimport silk.vector { make as vectorMake }\npub fn main() -> i32 { return 0 }'
// `make` is a prefix of neither alias, but the alias spellings repeat across the standard library,
// so the offsets are taken from the clause itself rather than searched for by spelling.
const vectorAliasBinding = stdlibAliases.indexOf('make as vectorMake') + 'make as '.length

it.effect('renames the project alias of a standard-library member the toolchain also aliases', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [{ module: 'main', text: stdlibAliases }],
      'main',
    )
    const position = positionAt(stdlibAliases, vectorAliasBinding)
    // `silk/bytes` and `silk/os_filesystem` spell their own alias of `silk.vector.make`
    // `vectorMake` too: identity and spelling both coincide, and only the module that wrote the
    // clause owns this binding.
    const renamed = Document.rename(document, snapshot, position, 'newVector', uriOfModule)
    assert.strictEqual(renamed?._tag, 'RenameEdit')
    if (renamed?._tag !== 'RenameEdit') return
    const changes = renamed.edit.changes ?? {}
    assert.deepEqual(Object.keys(changes), [uriOfModule('main')])
    assert.deepEqual(
      changes[uriOfModule('main')]?.map((edit) => ({ ...edit.range.start, newText: edit.newText })),
      [{ ...position, newText: 'newVector' }],
    )
    // No edit may reach the installation under any module name it ships.
    assert.isEmpty(Object.keys(changes).filter((uri) => uri.includes('/stdlib/')))
    // Prepare answers from the same facts, so the editor offers the rename it will accept.
    assert.deepEqual(Document.prepareRename(document, snapshot, position), {
      range: {
        start: position,
        end: { line: position.line, character: position.character + 'vectorMake'.length },
      },
      placeholder: 'vectorMake',
    })
    // Confining the *rename* leaves the read-only reference list whole: every occurrence of the
    // declaration, standard-library ones included, is still worth showing.
    const locations = Document.references(document, snapshot, position, true, uriOfModule)
    assert.isDefined(locations)
    assert.include(
      locations?.map(({ uri }) => uri),
      uriOfModule('main'),
    )
    assert.isNotEmpty(
      (locations ?? []).filter(({ uri }) => uri === Stdlib.find('silk/vector')?.sourceUrl.href),
    )
  }),
)

const alphaAlias = 'import geometry { area as region }\npub fn one() -> i32 { return region(1, 2) }'
const betaAlias = 'import geometry { area as region }\npub fn two() -> i32 { return region(3, 4) }'

it.effect('keeps one module alias rename out of another module that chose the same alias', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'alpha', text: alphaAlias },
        { module: 'beta', text: betaAlias },
      ],
      'alpha',
    )
    const binding = positionAt(alphaAlias, alphaAlias.indexOf('area as region') + 'area as '.length)
    const renamed = Document.rename(document, snapshot, binding, 'zone', uriOfModule)
    assert.strictEqual(renamed?._tag, 'RenameEdit')
    if (renamed?._tag !== 'RenameEdit') return
    const changes = renamed.edit.changes ?? {}
    // `beta` chose the same spelling for the same declaration; it is not `alpha`'s name to change.
    assert.deepEqual(Object.keys(changes), [uriOfModule('alpha')])
    assert.deepEqual(
      changes[uriOfModule('alpha')]?.map((edit) => ({
        ...edit.range.start,
        newText: edit.newText,
      })),
      [
        { ...binding, newText: 'zone' },
        { ...positionAt(alphaAlias, alphaAlias.indexOf('region(1, 2)')), newText: 'zone' },
      ],
    )
  }),
)

const wholeDocument = (text: string) => ({
  start: { line: 0, character: 0 },
  end: positionAt(text, text.length),
})

const redundantAlias =
  'import geometry as geometry\npub fn main() -> i32 { return geometry.area(1, 2) }'

it.effect('offers one quick fix that deletes a redundant alias clause', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'main', text: redundantAlias },
      ],
      'main',
    )
    const actions = Document.codeActions(
      document,
      snapshot,
      wholeDocument(redundantAlias),
      uriOfModule,
    )
    assert.strictEqual(actions.length, 1)
    const action = actions[0]
    assert.strictEqual(action?.title, 'Remove the redundant alias')
    assert.strictEqual(action?.kind, 'quickfix')
    // The action names the diagnostic it corrects, so the editor attaches it to that lightbulb.
    assert.strictEqual(action?.diagnostics?.[0]?.code, 'SEM0013')
    assert.deepEqual(action?.edit?.changes?.[uriOfModule('main')], [
      {
        range: {
          start: positionAt(redundantAlias, redundantAlias.indexOf(' as geometry')),
          end: positionAt(
            redundantAlias,
            redundantAlias.indexOf(' as geometry') + ' as geometry'.length,
          ),
        },
        newText: '',
      },
    ])
  }),
)

it.effect('offers no code action for a diagnostic that carries no edit', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> i32 { return missing() }'
    const { document, snapshot } = yield* open(source)
    assert.strictEqual(
      Document.diagnostics(document, snapshot, () => undefined)[0]?.code,
      'SEM0004',
    )
    assert.deepEqual(
      Document.codeActions(document, snapshot, wholeDocument(source), () => undefined),
      [],
    )
  }),
)

const inventoryOf = (
  modules: ReadonlyArray<ProjectModule>,
): WorkspaceInventory.WorkspaceInventory =>
  WorkspaceInventory.make({
    project: modules.map(({ module, text }) => {
      const source = SourceFile.make(module, encoder.encode(text))
      return [module, ModuleSummary.make(Parser.parse(Lexer.lex(source)))] as const
    }),
  })

it.effect('discovers ambiguous auto-import descriptors and resolves the selected new import', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> i32 { return calculate() }'
    const modules = [
      { module: 'main', text: source },
      { module: 'alpha', text: 'pub fn calculate() -> i32 { return 1 }' },
      { module: 'beta', text: 'pub fn calculate() -> i32 { return 2 }' },
      { module: 'private', text: 'fn calculate() -> i32 { return 3 }' },
    ]
    const { document, snapshot } = yield* open(source)
    const inventory = inventoryOf(modules)
    const actions = Document.codeActions(
      document,
      snapshot,
      wholeDocument(source),
      uriOfModule,
      inventory,
    )

    assert.deepEqual(
      actions.map((action) => action.title),
      ['Import calculate from alpha', 'Import calculate from beta'],
    )
    assert.deepEqual(actions[0]?.edit?.changes?.[document.uri], [
      {
        range: { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } },
        newText: 'import alpha { calculate }\n',
      },
    ])
    const selected = actions[0]
    assert.isDefined(selected)
    if (selected === undefined) return
    const resolved = Document.resolveCodeAction(
      document,
      snapshot,
      inventory,
      selected,
      uriOfModule,
    )
    assert.deepEqual(resolved.edit?.changes?.[document.uri], [
      {
        range: { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } },
        newText: 'import alpha { calculate }\n',
      },
    ])

    const stale = Document.make({ ...document, version: 2 })
    assert.include(
      Document.resolveCodeAction(stale, snapshot, inventory, selected, uriOfModule).disabled
        ?.reason ?? '',
      'revision',
    )
  }),
)

it.effect(
  'extends an existing import with UTF-16 ranges and filters actions outside the request',
  () =>
    Effect.gen(function* () {
      const library = `pub fn existing() -> i32 { return 1 }
pub fn calculate() -> i32 { return 2 }`
      const source = `// π🙂
import library { existing }
pub fn main() -> i32 { return calculate() }`
      const modules = [
        { module: 'library', text: library },
        { module: 'main', text: source },
      ]
      const { document, snapshot } = yield* openProject(modules, 'main')
      const inventory = inventoryOf(modules)
      assert.deepEqual(
        Document.codeActions(
          document,
          snapshot,
          { start: { line: 1, character: 0 }, end: { line: 1, character: 3 } },
          uriOfModule,
          inventory,
        ),
        [],
      )
      const actions = Document.codeActions(
        document,
        snapshot,
        wholeDocument(source),
        uriOfModule,
        inventory,
      )
      const action = actions.find((candidate) => candidate.title.includes('calculate'))
      assert.isDefined(action)
      if (action === undefined) return
      const resolved = Document.resolveCodeAction(
        document,
        snapshot,
        inventory,
        action,
        uriOfModule,
      )
      assert.deepEqual(resolved.edit?.changes?.[document.uri], [
        {
          range: {
            start: { line: 1, character: 'import library { existing'.length },
            end: { line: 1, character: 'import library { existing'.length },
          },
          newText: ', calculate',
        },
      ])
    }),
)

const twoAliases = `import geometry as geometry
import shapes as shapes
pub fn main() -> i32 { return geometry.area(1, 2) + shapes.sides() }`

it.effect('returns the quick fixes of two diagnostics in diagnostic order', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'shapes', text: 'pub fn sides() -> i32 { return 4 }' },
        { module: 'main', text: twoAliases },
      ],
      'main',
    )
    const actions = Document.codeActions(document, snapshot, wholeDocument(twoAliases), uriOfModule)
    assert.deepEqual(
      actions.map((action) => action.edit?.changes?.[uriOfModule('main')]?.[0]?.range.start),
      [
        positionAt(twoAliases, twoAliases.indexOf(' as geometry')),
        positionAt(twoAliases, twoAliases.indexOf(' as shapes')),
      ],
    )
  }),
)

const nonAsciiAlias = `// π🙂
import geometry as geometry
pub fn main() -> i32 { return geometry.area(1, 2) }`

it.effect('measures a quick fix range in UTF-16 units after non-ASCII source', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'main', text: nonAsciiAlias },
      ],
      'main',
    )
    const actions = Document.codeActions(
      document,
      snapshot,
      wholeDocument(nonAsciiAlias),
      uriOfModule,
    )
    // `positionAt` counts UTF-16 units, which is what the negotiated encoding measures; a byte
    // offset would place the clause later on its line.
    assert.deepEqual(actions[0]?.edit?.changes?.[uriOfModule('main')]?.[0]?.range, {
      start: positionAt(nonAsciiAlias, nonAsciiAlias.indexOf(' as geometry')),
      end: positionAt(nonAsciiAlias, nonAsciiAlias.indexOf(' as geometry') + ' as geometry'.length),
    })
  }),
)

it.effect('offers no quick fix for a diagnostic outside the requested range', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* openProject(
      [
        { module: 'geometry', text: geometry },
        { module: 'main', text: redundantAlias },
      ],
      'main',
    )
    assert.deepEqual(
      Document.codeActions(
        document,
        snapshot,
        { start: { line: 1, character: 0 }, end: { line: 1, character: 4 } },
        uriOfModule,
      ),
      [],
    )
  }),
)

const clamp = `pub fn clamp(value: i32, low: i32, high: i32) -> i32 { return value }
pub fn main() -> i32 { return clamp(3, 0, 10) }
`

it.effect('labels a call signature with its declaration and one label per parameter', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open(clamp)
    const help = Document.signatureHelp(
      document,
      snapshot,
      positionAt(clamp, clamp.indexOf('clamp(3') + 'clamp('.length),
    )
    assert.strictEqual(help?.signatures.length, 1)
    assert.strictEqual(
      help?.signatures[0]?.label,
      'pub fn clamp(value: i32, low: i32, high: i32) -> i32',
    )
    assert.deepEqual(
      help?.signatures[0]?.parameters?.map((parameter) => parameter.label),
      ['value: i32', 'low: i32', 'high: i32'],
    )
    assert.strictEqual(help?.activeSignature, 0)
  }),
)

it.effect('advances the active parameter across each comma of the call', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open(clamp)
    const call = clamp.indexOf('clamp(3')
    const activeAt = (offset: number) =>
      Document.signatureHelp(document, snapshot, positionAt(clamp, offset))?.activeParameter
    assert.strictEqual(activeAt(call + 'clamp('.length), 0)
    assert.strictEqual(activeAt(call + 'clamp(3, '.length), 1)
    assert.strictEqual(activeAt(call + 'clamp(3, 0, '.length), 2)
  }),
)

it.effect('returns no signature help outside a call', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open(clamp)
    assert.isUndefined(Document.signatureHelp(document, snapshot, positionOf(clamp, 'clamp', 0)))
    assert.isUndefined(
      Document.signatureHelp(
        document,
        snapshot,
        positionAt(clamp, clamp.indexOf('clamp(3, 0, 10)') + 'clamp(3, 0, 10)'.length),
      ),
    )
  }),
)

it.effect('answers signature help from the recovered call of a source with a parser error', () =>
  Effect.gen(function* () {
    const source = `pub fn clamp(value: i32, low: i32, high: i32) -> i32 { return value }
pub fn main() -> i32 { return clamp(3,
}
`
    const { document, snapshot } = yield* open(source)
    assert.isTrue(
      Document.diagnostics(document, snapshot, () => undefined).length > 0,
      'the source is expected not to compile',
    )
    const help = Document.signatureHelp(
      document,
      snapshot,
      positionAt(source, source.indexOf('clamp(3,') + 'clamp(3,'.length),
    )
    assert.strictEqual(
      help?.signatures[0]?.label,
      'pub fn clamp(value: i32, low: i32, high: i32) -> i32',
    )
    assert.strictEqual(help?.activeParameter, 1)
  }),
)

it.effect('carries the documentation of the called declaration as Markdown', () =>
  Effect.gen(function* () {
    const source = `/// Clamps a value into a range.
pub fn clamp(value: i32, low: i32, high: i32) -> i32 { return value }
pub fn main() -> i32 { return clamp(3, 0, 10) }
`
    const { document, snapshot } = yield* open(source)
    const help = Document.signatureHelp(
      document,
      snapshot,
      positionAt(source, source.indexOf('clamp(3') + 'clamp('.length),
    )
    assert.deepEqual(help?.signatures[0]?.documentation, {
      kind: 'markdown',
      value: 'Clamps a value into a range.',
    })
  }),
)

it.effect('selects the inner call when two calls nest', () =>
  Effect.gen(function* () {
    const source = `pub fn clamp(value: i32, low: i32, high: i32) -> i32 { return value }
pub fn double(value: i32) -> i32 { return value }
pub fn main() -> i32 { return clamp(double(1), 0, 10) }
`
    const { document, snapshot } = yield* open(source)
    const help = Document.signatureHelp(
      document,
      snapshot,
      positionAt(source, source.indexOf('double(1') + 'double('.length),
    )
    assert.strictEqual(help?.signatures[0]?.label, 'pub fn double(value: i32) -> i32')
    assert.strictEqual(help?.activeParameter, 0)
  }),
)

/** Decodes the protocol's delta encoding back into absolute, readable tokens. */
const decodeSemanticTokens = (tokens: {
  readonly data: ReadonlyArray<number>
}): ReadonlyArray<{
  readonly line: number
  readonly character: number
  readonly length: number
  readonly type: string
}> => {
  const decoded: Array<{
    line: number
    character: number
    length: number
    type: string
  }> = []
  let line = 0
  let character = 0
  for (let index = 0; index + 4 < tokens.data.length; index += 5) {
    const deltaLine = tokens.data[index] ?? 0
    const deltaCharacter = tokens.data[index + 1] ?? 0
    line += deltaLine
    character = deltaLine === 0 ? character + deltaCharacter : deltaCharacter
    decoded.push({
      line,
      character,
      length: tokens.data[index + 2] ?? 0,
      type: Document.semanticTokenTypes[tokens.data[index + 3] ?? 0] ?? 'unknown',
    })
  }
  return decoded
}

const semanticTokenAt = (
  decoded: ReadonlyArray<{
    readonly line: number
    readonly character: number
    readonly length: number
    readonly type: string
  }>,
  source: string,
  spelling: string,
  occurrence = 0,
) => {
  const { line, character } = positionOf(source, spelling, occurrence)
  return decoded.find((token) => token.line === line && token.character === character)
}

it.effect('colors a type name and a variable name with different token types', () =>
  Effect.gen(function* () {
    const source = `pub struct Point { x: i32 }
pub fn main() -> i32 {
  let total = 1
  return total
}
`
    const { document, snapshot } = yield* open(source)
    const decoded = decodeSemanticTokens(Document.semanticTokens(document, snapshot))
    const type = semanticTokenAt(decoded, source, 'Point')
    const variable = semanticTokenAt(decoded, source, 'total', 1)
    assert.strictEqual(type?.type, 'type')
    assert.strictEqual(variable?.type, 'variable')
    // The lexer's own kinds carry the rest, so a keyword never depends on a resolved name.
    assert.strictEqual(semanticTokenAt(decoded, source, 'pub')?.type, 'keyword')
    assert.strictEqual(semanticTokenAt(decoded, source, 'main')?.type, 'function')
  }),
)

it.effect('names every keyword the TextMate grammar colors in the semantic token legend', () =>
  Effect.gen(function* () {
    // The grammar and the legend both read the compiler's token kinds, so one source of keywords
    // decides both: every kind the grammar spells must reach a legend entry the server sends.
    const source = `${Object.values(TextMate.keywords).join(' ')}\n`
    const { document, snapshot } = yield* open(source)
    const decoded = decodeSemanticTokens(Document.semanticTokens(document, snapshot))
    const colored = new Set(
      decoded.filter((token) => token.type === 'keyword').map((token) => token.character),
    )
    assert.isTrue(Document.semanticTokenTypes.includes('keyword'))
    for (const spelling of Object.values(TextMate.keywords)) {
      assert.isTrue(
        colored.has(positionOf(source, spelling).character),
        `keyword ${spelling} carries no semantic token`,
      )
    }
    for (const type of Document.semanticTokenTypes) {
      assert.isTrue(
        (Object.values(SemanticTokenTypes) as ReadonlyArray<string>).includes(type),
        `legend entry ${type} is not a standard token type`,
      )
    }
  }),
)

it.effect('folds a function body from its open brace to its close brace', () =>
  Effect.gen(function* () {
    const source = `pub fn main() -> i32 {
  let total = 1
  return total
}
`
    const { document, snapshot } = yield* open(source)
    assert.deepEqual(
      Document.foldingRanges(document, snapshot).map(({ startLine, endLine }) => ({
        startLine,
        endLine,
      })),
      [{ startLine: 0, endLine: 3 }],
    )
  }),
)

it.effect('folds a run of comment lines as one block', () =>
  Effect.gen(function* () {
    const source = `// first
// second
// third
pub fn main() -> i32 { return 1 }
`
    const { document, snapshot } = yield* open(source)
    assert.deepEqual(Document.foldingRanges(document, snapshot), [
      { startLine: 0, endLine: 2, kind: 'comment' },
    ])
  }),
)

const calleeModule = 'pub fn helper(value: i32) -> i32 { return value }'

it.effect('lists two callers of one function across two modules', () =>
  Effect.gen(function* () {
    const first = 'import helper\npub fn caller() -> i32 { return helper.helper(1) }'
    const second = 'import helper\npub fn main() -> i32 { return helper.helper(3) }'
    const { document, snapshot } = yield* openProject(
      [
        { module: 'helper', text: calleeModule },
        { module: 'caller', text: first },
        { module: 'main', text: second },
      ],
      'helper',
    )
    const prepared = Document.prepareCallHierarchy(
      document,
      snapshot,
      positionOf(calleeModule, 'helper'),
      uriOfModule,
    )
    assert.strictEqual(prepared.length, 1)
    assert.strictEqual(prepared[0]?.name, 'helper')
    assert.strictEqual(prepared[0]?.detail, 'pub fn helper(value: i32) -> i32')
    const item = prepared[0]
    if (item === undefined) throw new Error('prepare produced no item')
    assert.deepEqual(
      Document.incomingCalls(document, snapshot, item, uriOfModule).map(({ from, fromRanges }) => ({
        name: from.name,
        uri: from.uri,
        calls: fromRanges.length,
      })),
      [
        { name: 'caller', uri: uriOfModule('caller'), calls: 1 },
        { name: 'main', uri: uriOfModule('main'), calls: 1 },
      ],
    )
  }),
)

it.effect('places structural answers with the negotiated position encoding', () =>
  Effect.gen(function* () {
    // The comment holds an astral character, which is one UTF-16 surrogate pair and four bytes:
    // a byte-counted column would put every answer after it two columns too far right.
    const source = `// 😀
pub fn helper(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  return helper(1)
}
`
    const { document, snapshot } = yield* open(source)
    const decoded = decodeSemanticTokens(Document.semanticTokens(document, snapshot))
    assert.deepEqual(
      decoded.filter((token) => token.line === 1 && token.type === 'function'),
      // `helper` sits at UTF-16 column 7 of its own line, unshifted by the astral character above.
      [{ line: 1, character: 7, length: 6, type: 'function' }],
    )
    assert.deepEqual(
      Document.foldingRanges(document, snapshot).map(({ startLine, endLine }) => ({
        startLine,
        endLine,
      })),
      [
        { startLine: 0, endLine: 0 },
        { startLine: 1, endLine: 1 },
        { startLine: 2, endLine: 4 },
      ].filter(({ startLine, endLine }) => endLine > startLine),
    )
    const prepared = Document.prepareCallHierarchy(
      document,
      snapshot,
      positionOf(source, 'main'),
      () => undefined,
    )
    const item = prepared[0]
    if (item === undefined) throw new Error('prepare produced no item')
    assert.deepEqual(
      Document.outgoingCalls(document, snapshot, item, () => undefined).map(
        ({ to, fromRanges }) => ({ name: to.name, ranges: fromRanges }),
      ),
      [
        {
          name: 'helper',
          ranges: [{ start: { line: 3, character: 9 }, end: { line: 3, character: 15 } }],
        },
      ],
    )
  }),
)

it.effect('lists the functions one function calls', () =>
  Effect.gen(function* () {
    const source = `pub fn helper(value: i32) -> i32 { return value }
pub fn double(value: i32) -> i32 { return value }
pub fn main() -> i32 { return helper(1) + double(2) + helper(3) }
`
    const { document, snapshot } = yield* open(source)
    const prepared = Document.prepareCallHierarchy(
      document,
      snapshot,
      positionOf(source, 'main'),
      () => undefined,
    )
    const item = prepared[0]
    if (item === undefined) throw new Error('prepare produced no item')
    assert.deepEqual(
      Document.outgoingCalls(document, snapshot, item, () => undefined).map(
        ({ to, fromRanges }) => ({ name: to.name, calls: fromRanges.length }),
      ),
      [
        { name: 'helper', calls: 2 },
        { name: 'double', calls: 1 },
      ],
    )
  }),
)
