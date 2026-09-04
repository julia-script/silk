import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('publishes target-sized Layout and checked repetition contracts', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'layout-value/contracts',
      ascii(`import silk.layout { Layout }
import silk.layout { LayoutOverflow }
fn repeat(layout: Layout, count: usize) -> Layout | LayoutOverflow {
  return Layout.repeat(move layout, count)
}
pub fn main() -> i32 { return 42 }`),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const catalog = Analysis.layoutCatalogOf(snapshot)
    assert.strictEqual(catalog._tag, 'Available')
    if (catalog._tag !== 'Available') return
    const layout = catalog.value.entries.find((entry) => Type.equals(entry.type, Type.layout))
    assert.strictEqual(layout?._tag, 'LayoutEntry')
    if (layout?._tag !== 'LayoutEntry') return
    assert.strictEqual(layout.size, 16)
    assert.strictEqual(layout.alignment, 8)
  }),
)
