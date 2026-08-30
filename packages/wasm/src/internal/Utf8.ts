/**
 * UTF-8 encoding without a dependency on platform globals, mirroring the workspace's
 * `ByteString` approach.
 *
 * @internal
 */
export const encode = (value: string): Uint8Array => {
  const bytes: Array<number> = []
  for (const character of value) {
    const codePoint = character.codePointAt(0) ?? 0
    const scalarValue = codePoint >= 0xd800 && codePoint <= 0xdfff ? 0xfffd : codePoint
    if (scalarValue <= 0x7f) {
      bytes.push(scalarValue)
    } else if (scalarValue <= 0x7ff) {
      bytes.push(0xc0 | (scalarValue >> 6), 0x80 | (scalarValue & 0x3f))
    } else if (scalarValue <= 0xffff) {
      bytes.push(
        0xe0 | (scalarValue >> 12),
        0x80 | ((scalarValue >> 6) & 0x3f),
        0x80 | (scalarValue & 0x3f),
      )
    } else {
      bytes.push(
        0xf0 | (scalarValue >> 18),
        0x80 | ((scalarValue >> 12) & 0x3f),
        0x80 | ((scalarValue >> 6) & 0x3f),
        0x80 | (scalarValue & 0x3f),
      )
    }
  }
  return Uint8Array.from(bytes)
}

/** Canonical identity for a string-derived WebAssembly name. */
export const canonicalKey = (value: string): string => {
  let key = ''
  for (const byte of encode(value)) key += String.fromCharCode(byte)
  return key
}
