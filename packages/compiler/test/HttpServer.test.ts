import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'

const encoder = new TextEncoder()
const reference = readFileSync(
  new URL('../../../apps/docs/content/reference/http-server.md', import.meta.url),
  'utf8',
)

it.effect(
  'realizes the reference example and canonical streaming HTTP server surface',
  () =>
    Effect.gen(function* () {
      const example = reference.match(/```silk\n([\s\S]*?)\n```/)?.[1]
      assert.isString(example)
      if (example === undefined) return
      const snapshot = yield* AnalysisFixture.retainingMain(
        'http-server/reference-example',
        encoder.encode(example),
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => ({
          code: diagnostic.code,
          message: diagnostic.message,
          sourceId: diagnostic.span.sourceId,
          start: diagnostic.span.start,
          end: diagnostic.span.end,
        })),
        [],
      )
    }),
  60_000,
)

it.effect(
  'publishes native admission only on supported socket targets',
  () =>
    Effect.gen(function* () {
      const source = `import silk.http_server_native {HttpServerNative}
pub fn main() -> i32 { return 42 }`
      const native = yield* AnalysisFixture.retainingMain(
        'http-server/native-admission-linux',
        encoder.encode(source),
        'x86_64-unknown-linux-gnu',
      )
      assert.deepEqual(Analysis.diagnostics(native), [])

      const wasm = yield* AnalysisFixture.retainingMain(
        'http-server/native-admission-wasm',
        encoder.encode(source),
        'wasm32-unknown-unknown',
      )
      const start = source.indexOf('HttpServerNative')
      assert.deepEqual(
        Analysis.diagnostics(wasm).map((diagnostic) => ({
          code: diagnostic.code,
          start: diagnostic.span.start,
          end: diagnostic.span.end,
        })),
        [{ code: 'SEM0014', start, end: start + 'HttpServerNative'.length }],
      )
    }),
  90_000,
)
