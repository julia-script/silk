import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'

const encoder = new TextEncoder()

const contractRejectionSource = `import silk.http_body {Completion, CompletionKind, Decoder}

fn mutateWhileCompletionBorrowed(mut decoder: Decoder) -> () {
  let evidence = Decoder.completion(&decoder)
  Decoder.abandon(&mut decoder)
  drop evidence
  return ()
}

fn forge<'owner>(marker: &'owner u8) -> Completion<'owner> {
  return Completion<'owner> {kindValue: CompletionKind.Delimited, marker: marker}
}`

it.effect(
  'checks the HTTP body reference declarations',
  () =>
    Effect.gen(function* () {
      const document = readFileSync(
        new URL('../../../apps/docs/content/reference/http-body-framing.md', import.meta.url),
        'utf8',
      )
      const source = document.match(/```silk\n([\s\S]*?)\n```/)?.[1]
      assert.isString(source)
      if (source === undefined) return
      const snapshot = yield* AnalysisFixture.declarations(
        'http-body/reference-example',
        encoder.encode(source),
      )
      const diagnostics = Analysis.diagnostics(snapshot)
      assert.deepEqual(diagnostics, [])
    }),
  20_000,
)

it.effect(
  'enforces borrowed completion ownership and opacity in one analysis',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.declarations(
        'http-body/contract-rejections',
        encoder.encode(contractRejectionSource),
      )
      const loanStart = contractRejectionSource.lastIndexOf('&mut decoder')
      assert.notStrictEqual(loanStart, -1)
      const construction =
        " Completion<'owner> {kindValue: CompletionKind.Delimited, marker: marker}"
      const constructionStart = contractRejectionSource.indexOf(construction)
      assert.notStrictEqual(constructionStart, -1)
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map(({ code, span }) => ({
          code,
          start: span.start,
          end: span.end,
        })),
        [
          { code: 'OWN0010', start: loanStart, end: loanStart + '&mut decoder'.length },
          {
            code: 'SEM0021',
            start: constructionStart,
            end: constructionStart + construction.length,
          },
        ],
      )
    }),
  20_000,
)
