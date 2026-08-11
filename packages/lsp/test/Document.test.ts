import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-effect/compiler/Analysis'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import * as Effect from 'effect/Effect'
import { SymbolKind } from 'vscode-languageserver-types'
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

const positionOf = (source: string, spelling: string, occurrence = 0) => {
  let offset = -1
  for (let index = 0; index <= occurrence; index += 1) offset = source.indexOf(spelling, offset + 1)
  const before = source.slice(0, offset)
  const lineStart = before.lastIndexOf('\n') + 1
  return { line: before.split('\n').length - 1, character: offset - lineStart }
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
