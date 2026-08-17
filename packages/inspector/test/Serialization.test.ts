import { Analysis } from '@silk-effect/compiler'
import * as Effect from 'effect/Effect'
import { describe, expect, it } from 'vitest'
import type { ViewContext } from '../src/Registry.js'
import { views } from '../src/Registry.js'

/**
 * Every view's result must survive structured serialization unchanged: the language server
 * answers projections over JSON-RPC, so a row that smuggles a function, a bigint, or a host
 * object would render in the docs workbench and silently break in the extension.
 */
describe('view results are serializable', () => {
  const source = `struct Pair { left: i32 right: i32 }
fn make() -> Pair { return Pair { right: 2, left: 1 } }
pub fn main() -> i32 { let pair = make() return pair.right }`
  const sourceId = 'memory/inspector/serialization'
  const snapshot = Effect.runSync(
    Analysis.ofSourceRealized(sourceId, new TextEncoder().encode(source), 'wasm32-unknown-unknown'),
  )
  const evaluation = Analysis.evaluate(snapshot)
  const context: ViewContext = {
    snapshot,
    modules: { [sourceId]: source },
    root: sourceId,
    mode: 'release',
    profile: 'release',
    evaluation,
    filter: '',
    showTrivia: false,
  }

  for (const view of views) {
    it(`round-trips the ${view.id} view through JSON`, () => {
      const result = view.project(context)
      expect(JSON.parse(JSON.stringify(result))).toEqual(result)
    })
  }

  it('qualifies every row span with its owning module', () => {
    for (const view of views) {
      for (const row of view.project(context).rows) {
        if (row.span !== undefined) {
          // Closure/index/resolution rows may point into stdlib modules, so the assertion is
          // that the module is present, not that it equals the root.
          expect(row.span.module, `${view.id}:${row.key}`).not.toBe('')
          expect(typeof row.span.module, `${view.id}:${row.key}`).toBe('string')
        }
      }
    }
  })
})
