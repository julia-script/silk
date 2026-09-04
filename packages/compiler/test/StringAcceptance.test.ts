import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'

const encoder = new TextEncoder()

it.effect('requires an unsafe boundary for unchecked UTF-8 formation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'string-acceptance/unsafe-authorization',
      encoder.encode(
        'fn view(bytes: &[u8]) -> string { return Intrinsic.stringFromUtf8Unchecked(bytes) }',
      ),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        reason: diagnostic.reason._tag,
      })),
      [{ code: 'SEM0082', reason: 'MissingUnsafeBoundary' }],
    )
  }),
)
