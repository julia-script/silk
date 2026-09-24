import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'
import { tomlValueAcceptanceSource } from './support/tomlValueAcceptance.js'

it.effect('analyzes owned TOML document use', () =>
  Effect.gen(function* () {
    const snapshot = yield* AnalysisFixture.retainingMain(
      'toml/document',
      Uint8Array.from(tomlValueAcceptanceSource, (character) => character.charCodeAt(0)),
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        message: diagnostic.message,
      })),
      [],
    )
  }),
)
