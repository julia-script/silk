import * as AnalysisFixture from './support/AnalysisFixture.js'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Type from '../src/Type.js'
import * as Mir from '../src/Mir.js'
import * as MirVerification from '../src/MirVerification.js'
import { unreachable } from './support/raise.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it.effect('publishes target-sized Layout and checked repetition contracts', () =>
  Effect.gen(function* () {
    const snapshot = yield* AnalysisFixture.retainingMain(
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

it.effect('returns ordinary layout measurement pairs and reconstructs source descriptors', () =>
  Effect.gen(function* () {
    const root = 'layout-value/ordinary-pairs'
    const snapshot = yield* AnalysisFixture.retainingMain(
      root,
      ascii(`import silk.layout { Layout }
struct Aligned { first: u8 second: u64 }
struct Empty {}
pub fn main() -> i32 {
  let scalar = Intrinsic.layoutOf<i32>()
  let aligned = Intrinsic.layoutOf<Aligned>()
  let empty = Intrinsic.layoutOf<Empty>()
  let scalarDescriptor = Layout.of<i32>()
  let alignedDescriptor = Layout.of<Aligned>()
  let emptyDescriptor = Layout.of<Empty>()
  drop scalar.0
  drop aligned.1
  drop empty.0
  drop scalarDescriptor.bytes
  drop alignedDescriptor.alignment
  drop emptyDescriptor.bytes
  return 42
}`),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const pairs = [...snapshot.index.generatedAggregates.values()].filter(
      (fact) => fact.identity?.module === root && fact.aggregateKind === 'AnonymousPositional',
    )
    assert.lengthOf(pairs, 3)
    for (const pair of pairs) {
      assert.strictEqual(pair.name._tag, 'Unavailable')
      assert.deepEqual(
        pair.fields.map((field) => [
          field.member,
          field.visibility,
          field.declaredType._tag === 'Resolved' ? field.declaredType.type : undefined,
        ]),
        [
          [{ _tag: 'OrdinalAggregateMember', ordinal: 0 }, 'Public', 'usize'],
          [{ _tag: 'OrdinalAggregateMember', ordinal: 1 }, 'Public', 'usize'],
        ],
      )
    }
    const program = Analysis.loweredMir(snapshot)
    assert.deepEqual(yield* MirVerification.verify(program), [])
    const measurements = (fn: Mir.MirFunction) => {
      const operations = fn.regions.flatMap((region) =>
        region._tag === 'OperationRegion' ? region.operations : [],
      )
      return operations.flatMap((operation) => {
        if (
          operation._tag !== 'Construct' ||
          !operation.type.type.name.startsWith('@AnonymousPositional:')
        )
          return []
        return [
          operation.fields.map(({ field, value }) => {
            const literal = operations.find(
              (candidate) =>
                candidate._tag === 'Literal' && candidate.destination.ordinal === value.ordinal,
            )
            if (literal?._tag !== 'Literal')
              return unreachable('measurement field must retain its target constant')
            assert.strictEqual(literal.type._tag, 'usize')
            return [field.ordinal, literal.value]
          }),
        ]
      })
    }
    const main = program.functions.find((fn) => fn.id.module === root && fn.id.name === 'main')
    if (main === undefined) return unreachable('expected the retained main')
    const expected = [
      [
        [0, 4n],
        [1, 4n],
      ],
      [
        [0, 16n],
        [1, 8n],
      ],
      [
        [0, 0n],
        [1, 1n],
      ],
    ]
    assert.deepEqual(measurements(main), expected)
    const wrappers = program.functions.filter(
      (fn) => fn.id.module === 'silk/layout' && fn.id.name === 'Layout.of',
    )
    assert.lengthOf(wrappers, 3)
    assert.sameDeepMembers(wrappers.flatMap(measurements), expected)
    const emitted = yield* Analysis.codegen(snapshot, { mode: 'release', verifyIr: true })
    assert.isAbove(emitted.bitcode.length, 0)
  }),
)
