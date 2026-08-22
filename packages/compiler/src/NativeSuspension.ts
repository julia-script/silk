import type * as Layout from './Layout.js'
import type * as Mir from './Mir.js'

/** Flattens the logical calling lanes retained across one suspension boundary. */
export const logicalLanes = (
  fn: Mir.MirFunction,
  locals: ReadonlyArray<Mir.LocalId>,
  lanesFor: (type: Mir.Type) => ReadonlyArray<Layout.CallingLane>,
): ReadonlyArray<Layout.CallingLane> =>
  Object.freeze(
    locals.flatMap((local) => {
      const type = fn.localTypes.at(local.ordinal)
      if (type === undefined) throw new RangeError(`LLVM suspension lost local %${local.ordinal}`)
      return lanesFor(type)
    }),
  )
