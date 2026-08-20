import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Layout from '../src/Layout.js'
import * as Type from '../src/Type.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const source = `import silk.usize as usize
struct Token { kind: i32 flag: bool }
struct Empty {}
fn scan(values: &[i32]) -> i32 { return usize.toI32(values.length) }
fn edit(values: &mut [i32]) -> i32 { return usize.toI32(values.length) }
fn tokens(values: &[Token]) -> i32 { return usize.toI32(values.length) }
fn empty(values: &[Empty]) -> i32 { return usize.toI32(values.length) }
pub fn main() -> i32 {
  let first = [1, 2, 3]
  let mut second = [1, 2, 3, 4, 5, 6]
  let tokenValues = [Token { kind: 1, flag: true }]
  let emptyValues = [Empty {}]
  let a = scan(&first)
  let b = edit(&mut second)
  let c = tokens(&tokenValues)
  let d = empty(&emptyValues)
  return a + b + c + d
}`

const snapshot = (target: 'wasm32-unknown-unknown' | 'x86_64-unknown-linux-gnu') =>
  Analysis.ofSourceRealized('slices/Layout', ascii(source), target)

it.effect('plans target-width address plus usize slice layouts and heterogeneous lanes', () =>
  Effect.gen(function* () {
    const wasm = yield* snapshot('wasm32-unknown-unknown')
    const native = yield* snapshot('x86_64-unknown-linux-gnu')
    const wasmPlan = Analysis.layoutOf(wasm)
    const nativePlan = Analysis.layoutOf(native)
    assert.strictEqual(wasmPlan._tag, 'Available')
    assert.strictEqual(nativePlan._tag, 'Available')
    if (wasmPlan._tag !== 'Available' || nativePlan._tag !== 'Available') return

    const shared = Type.slice('Shared', 'i32')
    const exclusive = Type.slice('Exclusive', 'i32')
    const wasmEntry = Layout.entry(wasmPlan.value, shared)
    const nativeEntry = Layout.entry(nativePlan.value, shared)
    assert.strictEqual(wasmEntry?.representation._tag, 'Slice')
    assert.strictEqual(nativeEntry?.representation._tag, 'Slice')
    if (
      wasmEntry?.representation._tag !== 'Slice' ||
      nativeEntry?.representation._tag !== 'Slice'
    ) {
      return
    }
    assert.deepEqual(
      [
        wasmEntry.size,
        wasmEntry.alignment,
        wasmEntry.representation.address.bits,
        wasmEntry.representation.length.offset,
      ],
      [8, 4, 32, 4],
    )
    assert.deepEqual(
      [
        nativeEntry.size,
        nativeEntry.alignment,
        nativeEntry.representation.address.bits,
        nativeEntry.representation.length.offset,
        nativeEntry.representation.tailPadding,
      ],
      [16, 8, 64, 8, 0],
    )
    assert.deepEqual(
      Layout.entry(wasmPlan.value, exclusive)?.representation,
      wasmEntry.representation,
    )

    const shape = Layout.callingShape(wasmPlan.value, shared)
    assert.strictEqual(shape?.tree._tag, 'SliceShape')
    assert.strictEqual(shape?.laneCount, 2)
    assert.deepEqual(
      shape?.lanes.map((lane) => ({
        type: typeof lane.type === 'string' ? lane.type : lane.type._tag,
        bits: typeof lane.type === 'string' ? undefined : lane.type.bits,
        selector: lane.path.at(0)?._tag,
      })),
      [
        { type: 'Address', bits: 32, selector: 'SliceAddressSelector' },
        { type: 'usize', bits: undefined, selector: 'SliceLengthSelector' },
      ],
    )
    assert.deepEqual(Layout.verify(wasmPlan.value), [])
    assert.deepEqual(Layout.verify(nativePlan.value), [])
  }),
)

it.effect('retains aggregate and zero-sized element stride independently of logical length', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('wasm32-unknown-unknown')
    const selected = Analysis.layoutOf(self)
    assert.strictEqual(selected._tag, 'Available')
    if (selected._tag !== 'Available') return

    const token = Type.nominal('slices/Layout', 'Token')
    const empty = Type.nominal('slices/Layout', 'Empty')
    const tokenSlice = Layout.entry(selected.value, Type.slice('Shared', token))
    const emptySlice = Layout.entry(selected.value, Type.slice('Shared', empty))
    assert.strictEqual(
      tokenSlice?.representation._tag === 'Slice' ? tokenSlice.representation.stride : undefined,
      8,
    )
    assert.strictEqual(
      emptySlice?.representation._tag === 'Slice' ? emptySlice.representation.stride : undefined,
      0,
    )
    assert.include(Layout.encode(selected.value), 'Address<i32,i32>')
  }),
)

it.effect('rejects a malformed address lane width', () =>
  Effect.gen(function* () {
    const self = yield* snapshot('wasm32-unknown-unknown')
    const selected = Analysis.layoutOf(self)
    assert.strictEqual(selected._tag, 'Available')
    if (selected._tag !== 'Available') return
    const type = Type.slice('Shared', 'i32')
    const shape = Layout.callingShape(selected.value, type)
    if (shape === undefined) throw new RangeError('missing shared slice shape')
    const malformedShape: Layout.CallingShape = Object.freeze({
      ...shape,
      lanes: Object.freeze([
        Object.freeze({
          _tag: 'CallingLane',
          path: shape.lanes.at(0)?.path ?? Object.freeze([]),
          type: Object.freeze({ _tag: 'Address', element: 'i32', bits: 64 }),
        }),
        ...shape.lanes.slice(1),
      ]),
    })
    const malformed: Layout.Plan = Object.freeze({
      ...selected.value,
      callingShapes: Object.freeze(
        selected.value.callingShapes.map((candidate) =>
          Type.equals(candidate.type, type) ? malformedShape : candidate,
        ),
      ),
    })

    assert.include(
      Layout.verify(malformed).map((violation) => violation.rule),
      'InvalidCallingShape',
    )
  }),
)
