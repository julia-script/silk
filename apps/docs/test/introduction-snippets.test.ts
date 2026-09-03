import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Analysis from '@silklang/compiler/Analysis'
import * as Effect from 'effect/Effect'

const introduction = readFileSync(
  new URL('../app/_introduction/introduction.html', import.meta.url),
  'utf8',
).replace(/<!--[\s\S]*?-->/g, '')

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
        'wasm32-unknown-unknown',
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
