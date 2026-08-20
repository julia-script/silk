import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as Target from '../src/Target.js'
import * as Type from '../src/Type.js'

const encoder = new TextEncoder()

const source = `import silk.usize as usize
fn inspect(text: string, bytes: &[u8]) -> usize {
  return Intrinsic.stringByteLength(text) + bytes.length
}
pub fn main() -> i32 {
  return usize.toI32(inspect("silk", b"silk"))
}`

const planned = (target: Target.Target) =>
  Analysis.ofSourceRealized('string/layout', encoder.encode(source), target.id)

it.effect('selects canonical string storage and calling lanes on every current target', () =>
  Effect.gen(function* () {
    for (const target of Target.all) {
      const snapshot = yield* planned(target)
      const selected = Analysis.layoutOf(snapshot)
      assert.strictEqual(selected._tag, 'Available')
      if (selected._tag !== 'Available') continue

      const entry = Layout.entry(selected.value, Type.string)
      assert.strictEqual(entry?.representation._tag, 'String')
      if (entry?.representation._tag !== 'String') continue
      assert.deepEqual(
        {
          size: entry.size,
          alignment: entry.alignment,
          provenance: entry.representation.storage.provenance,
          addressBits: entry.representation.storage.bits,
          byteLengthOffset: entry.representation.byteLength.offset,
          byteLengthSize: entry.representation.byteLength.size,
        },
        {
          size: target.pointerSize * 2,
          alignment: target.pointerAlignment,
          provenance: 'Utf8',
          addressBits: target.pointerSize === 4 ? 32 : 64,
          byteLengthOffset: target.pointerSize,
          byteLengthSize: target.pointerSize,
        },
      )

      const shape = Layout.callingShape(selected.value, Type.string)
      assert.strictEqual(shape?.tree._tag, 'StringShape')
      assert.strictEqual(shape?.laneCount, 2)
      assert.deepEqual(
        shape?.lanes.map((lane) => ({
          type: typeof lane.type === 'string' ? lane.type : lane.type._tag,
          provenance: typeof lane.type === 'string' ? undefined : Type.encode(lane.type.element),
          bits: typeof lane.type === 'string' ? undefined : lane.type.bits,
          selector: lane.path.at(0)?._tag,
          offset: Layout.laneOffset(selected.value, Type.string, lane.path),
        })),
        [
          {
            type: 'Address',
            provenance: 'string',
            bits: target.pointerSize === 4 ? 32 : 64,
            selector: 'StringStorageSelector',
            offset: 0,
          },
          {
            type: 'usize',
            provenance: undefined,
            bits: undefined,
            selector: 'StringByteLengthSelector',
            offset: target.pointerSize,
          },
        ],
      )
      assert.deepEqual(Layout.verify(selected.value), [])
      assert.include(Layout.encode(selected.value), 'layout string')
      assert.include(Layout.encode(selected.value), 'Address<string')
    }
  }),
)

it.effect('rejects slice representation and calling-shape facts forged for string', () =>
  Effect.gen(function* () {
    const snapshot = yield* planned(Target.wasm32UnknownUnknown)
    const selected = Analysis.layoutOf(snapshot)
    assert.strictEqual(selected._tag, 'Available')
    if (selected._tag !== 'Available') return

    const stringEntry = Layout.entry(selected.value, Type.string)
    const sliceType = Type.slice('Shared', 'u8')
    const sliceEntry = Layout.entry(selected.value, sliceType)
    const stringShape = Layout.callingShape(selected.value, Type.string)
    const sliceShape = Layout.callingShape(selected.value, sliceType)
    if (
      stringEntry === undefined ||
      sliceEntry === undefined ||
      stringShape === undefined ||
      sliceShape === undefined
    ) {
      throw new RangeError('expected reachable string and byte-slice layout facts')
    }

    const forgedEntry: Layout.Entry = Object.freeze({
      ...stringEntry,
      representation: sliceEntry.representation,
    })
    const forgedShape: Layout.CallingShape = Object.freeze({
      ...stringShape,
      tree: sliceShape.tree,
      lanes: sliceShape.lanes,
    })
    const malformed: Layout.Plan = Object.freeze({
      ...selected.value,
      entries: Object.freeze(
        selected.value.entries.map((entry) => (Type.isString(entry.type) ? forgedEntry : entry)),
      ),
      callingShapes: Object.freeze(
        selected.value.callingShapes.map((shape) =>
          Type.isString(shape.type) ? forgedShape : shape,
        ),
      ),
    })

    assert.include(
      Layout.verify(malformed).map((violation) => violation.rule),
      'InvalidAggregate',
    )
    assert.include(
      Layout.verify(malformed).map((violation) => violation.rule),
      'InvalidCallingShape',
    )
  }),
)
