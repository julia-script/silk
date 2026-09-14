import * as AnalysisFixture from './support/AnalysisFixture.js'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

it.effect(
  'checks the HTTP head reference declarations',
  () =>
    Effect.gen(function* () {
      const document = readFileSync(
        new URL('../../../apps/docs/content/reference/http-head-parsing.md', import.meta.url),
        'utf8',
      )
      const source = document.match(/```silk\n([\s\S]*?)\n```/)?.[1]
      assert.isString(source)
      if (source === undefined) return
      const snapshot = yield* AnalysisFixture.declarations(
        'http-head/reference-example',
        encoder.encode(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
    }),
  15_000,
)

it.effect(
  'rejects reset while a completed HTTP head view remains live',
  () =>
    Effect.gen(function* () {
      const source = `import silk.http_head {RequestParser}

fn resetWithLiveHead(mut parser: RequestParser) -> () {
  let head = RequestParser.head(&parser)
  let reset = RequestParser.reset(&mut parser)
  drop reset
  drop head
  return ()
}`
      const snapshot = yield* AnalysisFixture.declarations(
        'http-head/reset-live-head',
        encoder.encode(source),
      )
      const diagnostics = Analysis.diagnostics(snapshot)
      assert.deepEqual(
        diagnostics.map((diagnostic) => diagnostic.code),
        ['OWN0010'],
      )
      const diagnostic = diagnostics.at(0)
      assert.isDefined(diagnostic)
      if (diagnostic === undefined) return
      assert.strictEqual(source.slice(diagnostic.span.start, diagnostic.span.end), '&mut parser')
    }),
  15_000,
)
