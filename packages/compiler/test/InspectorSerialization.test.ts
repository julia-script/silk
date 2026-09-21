import * as Effect from 'effect/Effect'
import { it } from '@effect/vitest'
import { expect } from 'vitest'
import * as Analysis from '../src/Analysis.js'
import type { ViewContext } from '../src/InspectorRegistry.js'
import { views } from '../src/InspectorRegistry.js'

/**
 * Every view's result must survive structured serialization unchanged: the language server
 * answers projections over JSON-RPC, so a row that smuggles a function, a bigint, or a host
 * object would render in one static inspector client and silently break in another.
 */
it.effect('round-trips every inspector view through JSON with qualified spans', () =>
  Effect.gen(function* () {
    const source = `struct Pair { left: i32 right: i32 }
fn make() -> Pair { return Pair { right: 2, left: 1 } }
pub fn main() -> i32 { let pair = make() return pair.right }`
    const sourceId = 'memory/inspector/serialization'
    const snapshot = yield* Analysis.ofSourceRealized(
      sourceId,
      new TextEncoder().encode(source),
      'wasm32-unknown-unknown',
    )
    const context: ViewContext = {
      snapshot,
      modules: { [sourceId]: source },
      root: sourceId,
      mode: 'release',
      profile: 'release',
      filter: '',
      showTrivia: false,
    }

    for (const view of views) {
      const result = view.project(context)
      expect(JSON.parse(JSON.stringify(result)), view.id).toEqual(result)
      for (const row of result.rows) {
        if (row.span !== undefined) {
          // Closure/index/resolution rows may point into stdlib modules, so the assertion is
          // that the module is present, not that it equals the root.
          expect(row.span.module, `${view.id}:${row.key}`).not.toBe('')
          expect(typeof row.span.module, `${view.id}:${row.key}`).toBe('string')
        }
      }
    }
  }),
)
