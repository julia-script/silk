import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'
import { httpBodyAcceptanceSource } from './support/httpBodyAcceptance.js'

const encoder = new TextEncoder()

const contractRejectionSource = `${httpBodyAcceptanceSource}

effect fn mutateWhileCompletionBorrowed() -> () ! OutOfMemoryError ? &mut Allocator {
  let made = run decoderFor(
    b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nContent-Length: 0\\r\\n\\r\\n",
    bodyLimits(),
  )
  let mut decoder = match move made {
    Option.None => { return () }
    Option.Some {value} => move value
  }
  let evidence = Decoder.completion(&decoder)
  Decoder.abandon(&mut decoder)
  drop evidence
  return ()
}

fn forge<'owner>(marker: &'owner u8) -> Completion<'owner> {
  return Completion<'owner> {kindValue: CompletionKind.Delimited, marker: marker}
}`

it.effect(
  'compiles the HTTP body reference example',
  () =>
    Effect.gen(function* () {
      const document = readFileSync(
        new URL('../../../apps/docs/content/reference/http-body-framing.md', import.meta.url),
        'utf8',
      )
      const source = document.match(/```silk\n([\s\S]*?)\n```/)?.[1]
      assert.isString(source)
      if (source === undefined) return
      const snapshot = yield* AnalysisFixture.retainingMain(
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
      const snapshot = yield* AnalysisFixture.retainingMain(
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
