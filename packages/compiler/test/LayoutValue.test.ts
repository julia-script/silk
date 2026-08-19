import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Mir from '../src/Mir.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const program = (bytes: string, alignment: string): string => `pub fn main() -> i32 {
  let validated = Layout.make(${bytes}, ${alignment})
  return match move validated {
    Layout { bytes, alignment } => 42
    InvalidAlignment { alignment } => 0
  }
}`

it.effect('constructs valid zero-sized and over-aligned Layout values', () =>
  Effect.gen(function* () {
    for (const [bytes, alignment] of [
      ['0', '1'],
      ['0', '4096'],
      ['64', '8'],
    ] as const) {
      const snapshot = yield* Analysis.ofSourceRealized(
        `layout-value/valid-${bytes}-${alignment}`,
        ascii(program(bytes, alignment)),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed')
      assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 42n)
      assert.deepEqual(Mir.verify(Analysis.loweredMir(snapshot)), [])
    }
  }),
)

it.effect('returns ordinary InvalidAlignment data before allocation', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'layout-value/invalid-alignment',
      ascii(program('64', '3')),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    assert.strictEqual(evaluated._tag === 'Completed' ? evaluated.result.value : undefined, 0n)
  }),
)

it.effect('publishes target-sized Layout and checked repetition contracts', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'layout-value/contracts',
      ascii(`fn repeat(layout: Layout, count: usize) -> Layout | LayoutOverflow {
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
