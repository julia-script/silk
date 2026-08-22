import { alignUp } from './internal/Align.js'
import type * as Layout from './Layout.js'
import * as Scalar from './Scalar.js'
import type * as Target from './Target.js'

export const laneStorage = (
  target: Target.Target,
  lane: Layout.CallingLane,
): { readonly size: number; readonly alignment: number } => {
  if (typeof lane.type !== 'string')
    return Object.freeze({ size: target.pointerSize, alignment: target.pointerAlignment })
  const scalar = Scalar.find(lane.type)
  const bits = Scalar.bits(scalar ?? Scalar.defaultInteger, target.pointerSize === 4 ? 32 : 64)
  const size = bits / 8
  return Object.freeze({ size, alignment: Math.min(size, 8) })
}

/** Packs native calling lanes using their target ABI storage. */
export const packLanes = (
  target: Target.Target,
  lanes: ReadonlyArray<Layout.CallingLane>,
  start = 0,
): {
  readonly entries: ReadonlyArray<{ readonly lane: Layout.CallingLane; readonly offset: number }>
  readonly end: number
  readonly alignment: number
} => {
  let cursor = start
  let alignment = 1
  const entries = lanes.map((lane) => {
    const storage = laneStorage(target, lane)
    cursor = alignUp(cursor, storage.alignment)
    const entry = Object.freeze({ lane, offset: cursor })
    cursor += storage.size
    alignment = Math.max(alignment, storage.alignment)
    return entry
  })
  return Object.freeze({ entries: Object.freeze(entries), end: cursor, alignment })
}
