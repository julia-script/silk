import type * as Alignment from '../Alignment.js'

/** @internal */
class AlignmentEncodingFailure extends Error {
  constructor(readonly alignment: Alignment.Alignment) {
    super('LLVM instruction alignment exceeds the 6-bit bitcode encoding')
    this.name = 'AlignmentEncodingFailure'
  }
}

/** @internal */
export const alignmentCode = (alignment: Alignment.Alignment): number => {
  if (alignment.byteUnits === undefined) return 0
  let value = alignment.byteUnits
  let exponent = 0
  while (value > 1n) {
    value >>= 1n
    exponent += 1
  }
  if (exponent > 62) throw new AlignmentEncodingFailure(alignment)
  return exponent + 1
}
