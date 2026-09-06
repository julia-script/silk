import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Analysis from '@silklang/compiler/Analysis'
import * as Effect from 'effect/Effect'

const introductionSource = readFileSync(
  new URL('../app/_introduction/introduction.html', import.meta.url),
  'utf8',
)

const introduction = introductionSource.replace(/<!--[\s\S]*?-->/g, '')

const decodeHtml = (source: string): string =>
  source.replace(/&(amp|gt|lt|quot);/g, (_, entity: string) => {
    switch (entity) {
      case 'amp':
        return '&'
      case 'gt':
        return '>'
      case 'lt':
        return '<'
      case 'quot':
        return '"'
      default:
        return entity
    }
  })

interface LiveSnippet {
  readonly target: string
  readonly expectedDiagnosticCodes: ReadonlyArray<string>
  readonly source: string
}

const liveSnippets: ReadonlyArray<LiveSnippet> = Array.from(
  introduction.matchAll(
    /<silk-snippet(?<attributes>[^>]*)>(?<source>[\s\S]*?)<\/silk-snippet\s*>/g,
  ),
).flatMap((match) => {
  const { attributes, source } = match.groups ?? {}
  if (attributes?.includes('diagnostics') !== true || source === undefined) return []
  const expected = attributes.match(/expected-diagnostics="([^"]*)"/)?.[1] ?? ''
  return [
    {
      expectedDiagnosticCodes: expected === '' ? [] : expected.split(/\s+/),
      target: attributes.match(/target="([^"]*)"/)?.[1] ?? 'wasm32-unknown-unknown',
      source: decodeHtml(source).replace(/^\r?\n/, ''),
    },
  ]
})

it.effect('keeps every live landing-page example diagnostics-correct', () =>
  Effect.gen(function* () {
    for (const [index, snippet] of liveSnippets.entries()) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `landing-page/${index + 1}`,
        new TextEncoder().encode(snippet.source),
        snippet.target,
      )
      assert.deepStrictEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        snippet.expectedDiagnosticCodes,
        snippet.source,
      )
    }
  }),
)

it('keeps landing-page string literals on one source line', () => {
  for (const { source } of liveSnippets)
    assert.isNull(source.match(/"[^"\r\n]*[\r\n][^"]*"/), source)
})

it('protects snippet whitespace from the HTML formatter', () => {
  assert.strictEqual(
    introductionSource.match(/<!-- prettier-ignore -->\s*<silk-snippet\b/g)?.length ?? 0,
    introduction.match(/<silk-snippet\b/g)?.length ?? 0,
  )
})

it('keeps multi-statement snippets readable as source code', () => {
  assert.match(
    liveSnippets[3]?.source ?? '',
    /import silk\.effect \{ Effect \}\n\nstruct NotFoundError \{ id: i32 \}\n\neffect fn load/,
  )
})
