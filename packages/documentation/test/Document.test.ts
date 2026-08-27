import { assert, it } from '@effect/vitest'
import * as Analysis from '@silk-lang/compiler/Analysis'
import * as Effect from 'effect/Effect'
import * as Document from '../src/Document.js'
import * as Project from '../src/Project.js'

const encode = (value: string): Uint8Array => new TextEncoder().encode(value)

it.effect(
  'parses CommonMark, examples, links, and source provenance without compiler coupling',
  () =>
    Effect.gen(function* () {
      const source = `/// Recovers a [\`Problem\`] with **care**.
///
/// # Examples
/// \`\`\`silk
/// recover(problem)
/// \`\`\`
pub fn recover(problem: Problem) -> i32 { return problem.code }
`
      const snapshot = yield* Analysis.ofSource('docs/main', encode(source))
      const headers = Analysis.declarationIndex(snapshot).modules.at(0)
      const declaration = headers?.declarations.at(0)
      assert.isDefined(declaration)
      const raw = Analysis.documentationOfSyntax(snapshot, 'docs/main', declaration.syntax)
      assert.isDefined(raw)
      const syntax = Analysis.moduleAnalysis(snapshot, 'docs/main')?.syntax
      assert.isDefined(syntax)

      const parsed = Document.parse(syntax.source, raw)
      assert.strictEqual(parsed.fallback, false)
      assert.strictEqual(parsed.examples.length, 1)
      assert.strictEqual(parsed.examples[0]?.language, 'silk')
      assert.strictEqual(parsed.examples[0]?.code, 'recover(problem)')
      assert.strictEqual(parsed.source.sourceId, 'docs/main')
      assert.match(Document.toMarkdown(parsed), /`Problem`/)
      assert.match(Document.toMarkdown(parsed), /# Examples/)
    }),
)

it.effect('treats incomplete Markdown as documentation instead of a diagnostic', () =>
  Effect.gen(function* () {
    const source = `/// This **never closes and [neither does this
pub fn recover() -> i32 { return 1 }
`
    const snapshot = yield* Analysis.ofSource('malformed/main', encode(source))
    const declaration = Analysis.declarationIndex(snapshot).modules.at(0)?.declarations.at(0)
    assert.isDefined(declaration)
    const raw = Analysis.documentationOfSyntax(snapshot, 'malformed/main', declaration.syntax)
    const syntax = Analysis.moduleAnalysis(snapshot, 'malformed/main')?.syntax
    assert.isDefined(raw)
    assert.isDefined(syntax)

    const parsed = Document.parse(syntax.source, raw)
    assert.match(Document.toMarkdown(parsed), /never closes/)
  }),
)

it.effect('resolves known intra-doc links and leaves unknown links as inline code', () =>
  Effect.gen(function* () {
    const source = `/// Uses [\`Problem\`] and [\`Missing\`].
pub fn recover(problem: Problem) -> i32 { return problem.code }

pub struct Problem { pub code: i32 }
`
    const snapshot = yield* Analysis.ofSource('links/main', encode(source))
    const project = Project.make(snapshot)
    const documentation = project.modules[0]?.items[0]?.documentation
    assert.isDefined(documentation)
    const paragraph = documentation.blocks.at(0)
    assert.strictEqual(paragraph?._tag, 'Paragraph')
    if (paragraph?._tag !== 'Paragraph') return
    const links = paragraph.children.filter((child) => child._tag === 'SymbolLink')
    assert.strictEqual(links.length, 2)
    assert.strictEqual(links[0]?.target?.id, 'links/main::Problem')
    assert.isUndefined(links[1]?.target)
    assert.match(Document.toMarkdown(documentation), /`Missing`/)
  }),
)
