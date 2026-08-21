/** One canonical escape vocabulary for the Silk compiler. */

type ByteSequence = ReadonlyArray<number> | Uint8Array

const isContinuation = (byte: number | undefined): boolean =>
  byte !== undefined && (byte & 0xc0) === 0x80

/**
 * Counts the Unicode scalars one escaped body denotes, without decoding any of them.
 *
 * The count is a scalar count and never a byte count. One escape sequence denotes one scalar
 * however it is spelled. Only the extent of an escape matters here, never its meaning, so the
 * decoder in StaticText stays the single authority on what an escape produces.
 */
export const scalarCount = (bytes: ByteSequence, contentStart: number, end: number): number => {
  let index = contentStart
  let scalars = 0
  while (index < end) {
    scalars += 1
    if (bytes[index] !== 0x5c) {
      index += 1
      while (index < end && isContinuation(bytes[index])) index += 1
      continue
    }
    const escaped = bytes[index + 1]
    if (escaped === 0x75 && bytes[index + 2] === 0x7b) {
      index += 3
      while (index < end && bytes[index] !== 0x7d) index += 1
      index += 1
      continue
    }
    index += escaped === 0x78 ? 4 : 2
  }
  return scalars
}
