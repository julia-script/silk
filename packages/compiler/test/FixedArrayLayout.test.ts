import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as LayoutEncode from '../src/LayoutEncode.js'
import * as LayoutVerify from '../src/LayoutVerify.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const snapshot = (source: string) =>
  Analysis.ofSourceRealized('fixed-array-layout/main', ascii(source), 'wasm32-unknown-unknown')

it.effect('plans repeated layouts and canonical mixed selectors once before MIR', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Pair { left: i32 right: bool }
struct Bucket { pairs: [Pair; 2] tail: i32 }
fn choose(values: [Pair; 2], index: usize) -> i32 { return values[index].left }
pub fn main() -> i32 {
  let values = [Pair { left: 1, right: true }, Pair { left: 2, right: false }]
  return choose(move values, 1)
}`)
    assert.deepEqual(Analysis.diagnostics(self), [])
    const selected = Analysis.layoutOf(self)
    assert.strictEqual(selected._tag, 'Available')
    if (selected._tag !== 'Available') return
    const pair = Type.nominal('fixed-array-layout/main', 'Pair')
    const array = Type.fixedArray(pair, 2)
    const entry = Layout.entry(selected.value, array)
    assert.strictEqual(entry?.representation._tag, 'Repeated')
    if (entry?.representation._tag !== 'Repeated') return
    assert.deepEqual(
      [entry.size, entry.alignment, entry.representation.stride, entry.representation.length],
      [16, 4, 8, 2],
    )
    const shape = Layout.callingShape(selected.value, array)
    assert.strictEqual(shape?.tree._tag, 'RepeatedShape')
    assert.strictEqual(shape?.laneCount, 4)
    assert.deepEqual(
      shape?.lanes.map((lane) =>
        lane.path.map((selector) => {
          switch (selector._tag) {
            case 'ElementSelector':
              return `element:${selector.index}`
            case 'FieldId':
              return `field:${selector.ordinal}`
            case 'UnionTagSelector':
              return 'union:tag'
            case 'UnionPayloadSelector':
              return `union:payload:${selector.slot}`
            case 'SliceAddressSelector':
              return 'slice:address'
            case 'SliceLengthSelector':
              return 'slice:length'
            default:
              return assert.fail('unexpected layout selector')
          }
        }),
      ),
      [
        ['element:0', 'field:0'],
        ['element:0', 'field:1'],
        ['element:1', 'field:0'],
        ['element:1', 'field:1'],
      ],
    )
    assert.deepEqual(LayoutVerify.verify(selected.value), [])
  }),
)

it.effect('retains zero-length identity, element alignment, and zero lanes', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Token { kind: i32 }
fn empty() -> [Token; 0] { return [] }
pub fn main() -> i32 { let values = empty() return 0 }`)
    const selected = Analysis.layoutOf(self)
    assert.strictEqual(selected._tag, 'Available')
    if (selected._tag !== 'Available') return
    const type = Type.fixedArray(Type.nominal('fixed-array-layout/main', 'Token'), 0)
    const entry = Layout.entry(selected.value, type)
    const shape = Layout.callingShape(selected.value, type)
    assert.deepEqual(entry === undefined ? undefined : [entry.size, entry.alignment], [0, 4])
    assert.strictEqual(shape?.laneCount, 0)
    assert.deepEqual(shape?.lanes, [])
    assert.include(
      Analysis.instancesOf(self).instances.at(1)?.key.contractRow ?? [],
      `result:${Type.runtimeKey(type)}`,
    )
  }),
)

it.effect('keeps unused arrays out of the runtime plan and retains overflow as catalog data', () =>
  Effect.gen(function* () {
    const self = yield* snapshot(`struct Huge { values: [[i32; 2147483647]; 2147483647] }
fn unused(values: [bool; 8]) -> [bool; 8] { return values }
pub fn main() -> i32 { return 0 }`)
    const catalog = Analysis.layoutCatalogOf(self)
    const plan = Analysis.layoutOf(self)
    assert.strictEqual(catalog._tag, 'Available')
    assert.strictEqual(plan._tag, 'Available')
    if (catalog._tag !== 'Available' || plan._tag !== 'Available') return
    const huge = Type.fixedArray(Type.fixedArray('i32', 2147483647), 2147483647)
    const unavailable = Layout.catalogEntry(catalog.value, huge)
    assert.strictEqual(unavailable?._tag, 'UnavailableLayoutEntry')
    assert.strictEqual(
      plan.value.entries.some((entry) => Type.equals(entry.type, Type.fixedArray('bool', 8))),
      false,
    )
    assert.strictEqual(
      LayoutEncode.encodeCatalog(catalog.value),
      LayoutEncode.encodeCatalog(catalog.value),
    )
  }),
)
