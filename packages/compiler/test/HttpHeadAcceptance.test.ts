import * as AnalysisFixture from './support/AnalysisFixture.js'
import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

it.effect(
  'compiles the HTTP head reference example',
  () =>
    Effect.gen(function* () {
      const document = readFileSync(
        new URL('../../../apps/docs/content/reference/http-head-parsing.md', import.meta.url),
        'utf8',
      )
      const source = document.match(/```silk\n([\s\S]*?)\n```/)?.[1]
      assert.isString(source)
      if (source === undefined) return
      const snapshot = yield* AnalysisFixture.retainingMain(
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
      const source = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.http_head { Limits, ParseError, Progress, RequestHead, RequestParser }
import silk.http_headers { Limits as ValueLimits }
import silk.result { Result }

effect fn program() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let limits = Limits {
    maxHeadBytes: 64,
    maxStartLineBytes: 32,
    maxFieldLineBytes: 32,
    maxOwnedBytes: 96,
    values: ValueLimits {
      maxMethodBytes: 8,
      maxTargetBytes: 8,
      maxNameBytes: 8,
      maxValueBytes: 8,
      maxFields: 1,
      maxFieldBytes: 16,
      maxOwnedBytes: 64,
    },
  }
  let made = run RequestParser.make(limits)
  let mut parser = match move made {
    Result<RequestParser, ParseError>.Failure {error} => { return 1 }
    Result<RequestParser, ParseError>.Success {value} => move value
  }
  let fed = RequestParser.feed(&mut parser, b"GET / HTTP/1.0\\r\\n\\r\\n", true)
  drop fed
  let head = RequestParser.head(&parser)
  let reset = RequestParser.reset(&mut parser)
  drop reset
  drop head
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run Effect.catchAll(
    program() |> Effect.provideMut<Allocator>(&mut allocator),
    recover,
  )
}`
      const snapshot = yield* AnalysisFixture.retainingMain(
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
