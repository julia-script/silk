import type * as LlvmBlock from '@silk-effect/llvm/Block'
import type * as Mir from './Mir.js'

/** Resolves one MIR control target to its declared LLVM block. */
export const targetBlock = (
  blocks: ReadonlyMap<number, LlvmBlock.Block>,
  target: Mir.RegionId,
  operation: string,
): LlvmBlock.Block => {
  const block = blocks.get(target.ordinal)
  if (block === undefined) throw new RangeError(`${operation} targets a missing block`)
  return block
}
