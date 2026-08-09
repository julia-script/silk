/** Compiler-owned immutable data decoded from one static text or byte-string literal. */
export interface Data {
  readonly _tag: 'StaticData'
  readonly id: string
  readonly kind: 'Text' | 'Bytes'
  readonly bytes: ReadonlyArray<number>
  readonly utf8: boolean
}

export type DecodeResult =
  | { readonly _tag: 'Decoded'; readonly data: Data }
  | { readonly _tag: 'Invalid'; readonly detail: string; readonly offset: number }

const hex = (bytes: ReadonlyArray<number>): string =>
  bytes.map((byte) => byte.toString(16).padStart(2, '0')).join('')

const decoded = (kind: Data['kind'], bytes: ReadonlyArray<number>, utf8: boolean): DecodeResult =>
  Object.freeze({
    _tag: 'Decoded',
    data: Object.freeze({
      _tag: 'StaticData',
      id: `${kind === 'Text' ? 'text' : 'bytes'}:${hex(bytes)}`,
      kind,
      bytes: Object.freeze([...bytes]),
      utf8,
    }),
  })

const invalid = (detail: string, offset: number): DecodeResult =>
  Object.freeze({ _tag: 'Invalid', detail, offset })

const hexDigit = (byte: number | undefined): number | undefined => {
  if (byte !== undefined && byte >= 0x30 && byte <= 0x39) return byte - 0x30
  if (byte !== undefined && byte >= 0x41 && byte <= 0x46) return byte - 0x41 + 10
  if (byte !== undefined && byte >= 0x61 && byte <= 0x66) return byte - 0x61 + 10
  return undefined
}

const utf8 = (scalar: number): ReadonlyArray<number> =>
  Object.freeze(Array.from(new TextEncoder().encode(String.fromCodePoint(scalar))))

/** Decodes a complete literal token once, preserving exact bytes for all later phases. */
export const decode = (token: ReadonlyArray<number>, byteString: boolean): DecodeResult => {
  const contentStart = byteString ? 2 : 1
  if (token.at(contentStart - 1) !== 0x22 || token.at(-1) !== 0x22)
    return invalid('unterminated literal', Math.max(0, token.length - 1))
  const output: Array<number> = []
  let index = contentStart
  const end = token.length - 1
  while (index < end) {
    const byte = token[index]
    if (byte !== 0x5c) {
      if (byte !== undefined) output.push(byte)
      index += 1
      continue
    }
    const escapeOffset = index
    index += 1
    const escaped = token[index]
    index += 1
    if (escaped === 0x6e) output.push(0x0a)
    else if (escaped === 0x72) output.push(0x0d)
    else if (escaped === 0x74) output.push(0x09)
    else if (escaped === 0x30) output.push(0)
    else if (escaped === 0x22 || escaped === 0x5c) output.push(escaped)
    else if (escaped === 0x78) {
      const high = hexDigit(token[index])
      const low = hexDigit(token[index + 1])
      if (high === undefined || low === undefined)
        return invalid('`\\x` escape requires exactly two hexadecimal digits', escapeOffset)
      output.push(high * 16 + low)
      index += 2
    } else if (escaped === 0x75) {
      if (token[index] !== 0x7b) return invalid('`\\u` escape requires `{...}`', escapeOffset)
      index += 1
      let scalar = 0
      let digits = 0
      while (index < end && token[index] !== 0x7d) {
        const digit = hexDigit(token[index])
        if (digit === undefined || digits === 6)
          return invalid('invalid Unicode scalar escape', escapeOffset)
        scalar = scalar * 16 + digit
        digits += 1
        index += 1
      }
      if (
        digits === 0 ||
        token[index] !== 0x7d ||
        scalar > 0x10ffff ||
        (scalar >= 0xd800 && scalar <= 0xdfff)
      )
        return invalid('invalid Unicode scalar escape', escapeOffset)
      index += 1
      if (byteString) {
        if (scalar > 0xff)
          return invalid('byte-string escape is outside the u8 range', escapeOffset)
        output.push(scalar)
      } else output.push(...utf8(scalar))
    } else return invalid('unknown escape sequence', escapeOffset)
  }
  if (byteString) return decoded('Bytes', output, false)
  try {
    new TextDecoder('utf-8', { fatal: true }).decode(Uint8Array.from(output))
  } catch {
    return invalid('text literal is not valid UTF-8', contentStart)
  }
  return decoded('Text', output, true)
}
