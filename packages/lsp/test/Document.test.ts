import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-effect/compiler/Analysis'
import * as Effect from 'effect/Effect'
import { SymbolKind } from 'vscode-languageserver-types'
import * as Document from '../src/Document.js'

const encoder = new TextEncoder()

const open = (
  text: string,
): Effect.Effect<{
  readonly document: Document.Document
  readonly snapshot: Analysis.Snapshot
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

it.effect('publishes compiler errors as protocol diagnostics', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> I32 { return missing() }'
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

it.effect('reports no diagnostics for a valid program', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open('pub fn main() -> I32 { return 42 }')
    assert.deepEqual(
      Document.diagnostics(document, snapshot, () => undefined),
      [],
    )
  }),
)

it.effect('hovers the type of the smallest enclosing expression', () =>
  Effect.gen(function* () {
    const source = 'pub fn main() -> I32 { return 42 }'
    const { document, snapshot } = yield* open(source)
    const hover = Document.hover(document, snapshot, {
      line: 0,
      character: source.indexOf('42'),
    })
    assert.isDefined(hover)
    assert.deepEqual(hover?.contents, { kind: 'markdown', value: '```silk\nI32\n```' })
  }),
)

it.effect('hovers nothing outside typed expressions', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open('pub fn main() -> I32 { return 42 }')
    assert.isUndefined(Document.hover(document, snapshot, { line: 0, character: 0 }))
  }),
)

it.effect('lists functions and structs with fields as document symbols', () =>
  Effect.gen(function* () {
    const source = `pub struct Box { answer: I32 }
pub fn main() -> I32 { return 42 }`
    const { document, snapshot } = yield* open(source)
    const symbols = Document.symbols(document, snapshot)
    assert.deepEqual(
      symbols.map((symbol) => [symbol.name, symbol.kind]),
      [
        ['Box', SymbolKind.Struct],
        ['main', SymbolKind.Function],
      ],
    )
    assert.deepEqual(
      symbols[0]?.children?.map((child) => [child.name, child.kind]),
      [['answer', SymbolKind.Field]],
    )
  }),
)

it.effect('formats a non-canonical document with one whole-document edit', () =>
  Effect.gen(function* () {
    const { document, snapshot } = yield* open('pub fn main() -> I32 { return   42 }')
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
