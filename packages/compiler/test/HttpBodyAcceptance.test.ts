import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'
import { httpBodyAcceptanceSource } from './support/httpBodyAcceptance.js'

const encoder = new TextEncoder()

const borrowedResetSource = `${httpBodyAcceptanceSource}

effect fn resetWhileCompletionBorrowed() -> () ! OutOfMemoryError ? &mut Allocator {
  let made = run decoderFor(
    b"POST / HTTP/1.1\\r\\nHost: example.com\\r\\nContent-Length: 0\\r\\n\\r\\n",
    bodyLimits(),
  )
  let mut decoder = match move made {
    Option.None => { return () }
    Option.Some {value} => move value
  }
  let evidence = Decoder.completion(&decoder)
  let reset = Decoder.reset(&mut decoder, Framing.Empty)
  drop reset
  drop evidence
  return ()
}`

const opaqueCompletionSource = `import silk.http_body { Completion, CompletionKind }

fn forge<'owner>(marker: &'owner u8) -> Completion<'owner> {
  return Completion<'owner> {kindValue: CompletionKind.Delimited, marker: marker}
}

pub fn main() -> i32 { return 0 }`

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
  'rejects reset while completion evidence remains borrowed',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-body/borrowed-reset',
        encoder.encode(borrowedResetSource),
      )
      const start = borrowedResetSource.lastIndexOf('&mut decoder')
      assert.notStrictEqual(start, -1)
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map(({ code, span }) => ({
          code,
          start: span.start,
          end: span.end,
        })),
        [{ code: 'OWN0010', start, end: start + '&mut decoder'.length }],
      )
    }),
  20_000,
)

it.effect(
  'keeps completion evidence opaque to application code',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-body/opaque-completion',
        encoder.encode(opaqueCompletionSource),
      )
      const construction =
        " Completion<'owner> {kindValue: CompletionKind.Delimited, marker: marker}"
      const start = opaqueCompletionSource.indexOf(construction)
      assert.notStrictEqual(start, -1)
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map(({ code, span }) => ({
          code,
          start: span.start,
          end: span.end,
        })),
        [{ code: 'SEM0021', start, end: start + construction.length }],
      )
    }),
  20_000,
)

it.effect(
  'realizes the portable HTTP body acceptance program through the public module',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-body/acceptance',
        encoder.encode(httpBodyAcceptanceSource),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
    }),
  20_000,
)
