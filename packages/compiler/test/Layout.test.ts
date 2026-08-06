import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('plans only concrete types reached through discovered instances', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'layout/program',
      ascii(`pub fn unused(value: Bool) -> Bool { return value }
pub fn main() -> I32 { return 42 }`),
    )
    const catalog = Layout.catalog(Target.aarch64AppleDarwin, Analysis.declarationIndex(snapshot))
    const plan = Layout.plan(catalog, Analysis.instancesOf(snapshot))

    assert.deepEqual(
      plan.entries.map((candidate) => candidate.type),
      ['I32'],
    )
    assert.deepEqual(Layout.verify(plan), [])
  }),
)

it('orders and encodes canonical scalar entries identically on every target', () => {
  for (const target of Target.all) {
    const first = Layout.make(target, ['I32', 'Bool', 'I32'])
    const second = Layout.make(target, ['Bool', 'I32'])
    assert.deepEqual(
      first.entries.map((candidate) => candidate.type),
      ['Bool', 'I32'],
    )
    assert.strictEqual(Layout.encode(first), Layout.encode(second))
    assert.deepEqual(Layout.verify(first), [])
  }
})

it('reports malformed target, order, duplicates, and scalar facts as data', () => {
  const canonical = Layout.make(Target.aarch64AppleDarwin, ['Bool', 'I32'])
  const bool = canonical.entries.at(0)
  const i32 = canonical.entries.at(1)
  if (bool === undefined || i32 === undefined) throw new Error('expected scalar layouts')
  const malformed: Layout.Plan = {
    ...canonical,
    target: { ...Target.aarch64AppleDarwin, pointerSize: 4 },
    entries: [i32, { ...bool, size: 1 }, bool],
  }

  assert.deepEqual(
    Layout.verify(malformed).map((violation) => violation.rule),
    [
      'NonCanonicalTarget',
      'NonCanonicalOrder',
      'InvalidScalar',
      'DuplicateType',
      'InvalidCallingShape',
    ],
  )
})

it.effect(
  'catalogs empty and nested structs before reachability and reuses their exact entries',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSource(
        'layout/catalog',
        ascii(`struct Empty {}
struct Pair { left: I32 right: Bool }
struct Outer { marker: Empty pair: Pair }
struct Unused { value: I32 }
fn make() -> Outer { return Outer { marker: Empty {}, pair: Pair { right: true, left: 42 } } }
pub fn main() -> I32 { let outer = make() return outer.pair.left }`),
        'wasm32-unknown-unknown',
      )
      const catalog = Analysis.layoutCatalogOf(snapshot)
      const plan = Analysis.layoutOf(snapshot)
      assert.strictEqual(catalog._tag, 'Available')
      assert.strictEqual(plan._tag, 'Available')
      if (catalog._tag !== 'Available' || plan._tag !== 'Available') return

      assert.deepEqual(
        catalog.value.entries.map((candidate) => Type.encode(candidate.type)),
        [
          'layout/catalog.Empty',
          'layout/catalog.Outer',
          'layout/catalog.Pair',
          'layout/catalog.Unused',
        ],
      )
      const empty = Layout.catalogEntry(catalog.value, Type.nominal('layout/catalog', 'Empty'))
      const pair = Layout.catalogEntry(catalog.value, Type.nominal('layout/catalog', 'Pair'))
      const outer = Layout.catalogEntry(catalog.value, Type.nominal('layout/catalog', 'Outer'))
      assert.strictEqual(empty?._tag, 'LayoutEntry')
      assert.strictEqual(pair?._tag, 'LayoutEntry')
      assert.strictEqual(outer?._tag, 'LayoutEntry')
      if (
        empty?._tag !== 'LayoutEntry' ||
        pair?._tag !== 'LayoutEntry' ||
        outer?._tag !== 'LayoutEntry' ||
        pair.representation._tag !== 'Aggregate' ||
        outer.representation._tag !== 'Aggregate'
      )
        return
      assert.deepEqual([empty.size, empty.alignment], [0, 1])
      assert.deepEqual(
        pair.representation.fields.map((field) => [field.name, field.offset, field.padding]),
        [
          ['left', 0, 0],
          ['right', 4, 0],
        ],
      )
      assert.deepEqual([pair.size, pair.alignment, pair.representation.tailPadding], [8, 4, 0])
      assert.deepEqual(
        outer.representation.fields.map((field) => [field.name, field.offset, field.size]),
        [
          ['marker', 0, 0],
          ['pair', 0, 8],
        ],
      )
      assert.deepEqual(Layout.verifyCatalog(catalog.value), [])
      assert.deepEqual(Layout.verifyAgainstCatalog(plan.value, catalog.value), [])
      assert.strictEqual(Layout.entry(plan.value, Type.nominal('layout/catalog', 'Outer')), outer)
      assert.strictEqual(
        Layout.entry(plan.value, Type.nominal('layout/catalog', 'Unused')),
        undefined,
      )
    }),
)

it.effect(
  'retains unavailable fields, cycles, and transitive dependencies without harming peers',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSource(
        'layout/unavailable',
        ascii(`struct Good { value: I32 }
struct Broken { value: Missing }
struct Outer { broken: Broken }
struct Left { right: Right }
struct Right { left: Left }
pub fn main() -> I32 { return 42 }`),
        'aarch64-apple-darwin',
      )
      const selected = Analysis.layoutCatalogOf(snapshot)
      assert.strictEqual(selected._tag, 'Available')
      if (selected._tag !== 'Available') return
      const get = (name: string) =>
        Layout.catalogEntry(selected.value, Type.nominal('layout/unavailable', name))
      const good = get('Good')
      const broken = get('Broken')
      const outer = get('Outer')
      const left = get('Left')
      const right = get('Right')

      assert.strictEqual(good?._tag, 'LayoutEntry')
      assert.strictEqual(broken?._tag, 'UnavailableLayoutEntry')
      assert.strictEqual(outer?._tag, 'UnavailableLayoutEntry')
      assert.strictEqual(left?._tag, 'UnavailableLayoutEntry')
      assert.strictEqual(right?._tag, 'UnavailableLayoutEntry')
      if (
        broken?._tag !== 'UnavailableLayoutEntry' ||
        outer?._tag !== 'UnavailableLayoutEntry' ||
        left?._tag !== 'UnavailableLayoutEntry' ||
        right?._tag !== 'UnavailableLayoutEntry'
      )
        return
      assert.strictEqual(broken.cause?.code, 'SEM0001')
      assert.strictEqual(outer.reason._tag, 'UnavailableDependency')
      assert.strictEqual(outer.cause?.code, 'SEM0001')
      assert.strictEqual(left.cause?.code, 'SEM0020')
      assert.strictEqual(right.cause?.code, 'SEM0020')
      assert.deepEqual(Layout.verifyCatalog(selected.value), [])
    }),
)

it.effect(
  'encodes nominal catalogs identically for repeated inputs and preserves target identity',
  () =>
    Effect.gen(function* () {
      const source = ascii(
        'struct Pair { left: I32 right: Bool }\npub fn main() -> I32 { return 42 }',
      )
      const first = yield* Analysis.ofSource('layout/repeat', source, 'aarch64-apple-darwin')
      const second = yield* Analysis.ofSource('layout/repeat', source, 'aarch64-apple-darwin')
      const wasm = yield* Analysis.ofSource('layout/repeat', source, 'wasm32-unknown-unknown')
      const firstCatalog = Analysis.layoutCatalogOf(first)
      const secondCatalog = Analysis.layoutCatalogOf(second)
      const wasmCatalog = Analysis.layoutCatalogOf(wasm)
      assert.strictEqual(firstCatalog._tag, 'Available')
      assert.strictEqual(secondCatalog._tag, 'Available')
      assert.strictEqual(wasmCatalog._tag, 'Available')
      if (
        firstCatalog._tag !== 'Available' ||
        secondCatalog._tag !== 'Available' ||
        wasmCatalog._tag !== 'Available'
      )
        return
      assert.strictEqual(
        Layout.encodeCatalog(firstCatalog.value),
        Layout.encodeCatalog(secondCatalog.value),
      )
      assert.notStrictEqual(
        Layout.encodeCatalog(firstCatalog.value),
        Layout.encodeCatalog(wasmCatalog.value),
      )
    }),
)

it.effect('reports malformed aggregate facts and divergence from the catalog', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'layout/verify-aggregate',
      ascii('struct Pair { left: I32 right: Bool }\npub fn main() -> Pair { return 42 }'),
      'aarch64-apple-darwin',
    )
    const selected = Analysis.layoutCatalogOf(snapshot)
    assert.strictEqual(selected._tag, 'Available')
    if (selected._tag !== 'Available') return
    const pair = Layout.catalogEntry(
      selected.value,
      Type.nominal('layout/verify-aggregate', 'Pair'),
    )
    assert.strictEqual(pair?._tag, 'LayoutEntry')
    if (pair?._tag !== 'LayoutEntry' || pair.representation._tag !== 'Aggregate') return
    const first = pair.representation.fields.at(0)
    if (first === undefined) return
    const malformed: Layout.Entry = {
      ...pair,
      representation: {
        ...pair.representation,
        fields: [{ ...first, offset: 1 }, ...pair.representation.fields.slice(1)],
      },
    }
    const catalog: Layout.Catalog = { ...selected.value, entries: [malformed] }
    const plan: Layout.Plan = {
      _tag: 'LayoutPlan',
      target: selected.value.target,
      entries: [malformed],
      callingShapes: [],
    }

    assert.include(
      Layout.verifyCatalog(catalog).map((violation) => violation.rule),
      'InvalidAggregate',
    )
    assert.deepEqual(
      Layout.verifyAgainstCatalog(plan, selected.value).map((violation) => violation.rule),
      ['CatalogMismatch'],
    )
  }),
)
