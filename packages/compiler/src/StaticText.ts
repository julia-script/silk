import * as Escape from './internal/Escape.js'
import type * as LiteralForm from './LiteralForm.js'

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

const invalid = (detail: string, offset: number) =>
  Object.freeze({ _tag: 'Invalid' as const, detail, offset })

/** Decodes a complete literal token once, preserving exact bytes for all later phases. */
export const decode = (
  token: ReadonlyArray<number>,
  form: LiteralForm.LiteralForm,
): DecodeResult => {
  const contentStart = form.modifier.length + form.delimiterWidth
  const end = token.length - form.delimiterWidth
  const openingIsValid = Array.from(
    { length: form.delimiterWidth },
    (_, index) => token[form.modifier.length + index] === form.delimiter,
  ).every(Boolean)
  const closingIsValid = Array.from(
    { length: form.delimiterWidth },
    (_, index) => token[end + index] === form.delimiter,
  ).every(Boolean)
  if (!openingIsValid || !closingIsValid || end < contentStart)
    return invalid('unterminated literal', Math.max(0, token.length - 1))
  const byteString = form.category === 'Bytes'
  const output: Array<number> = []
  let index = contentStart
  while (index < end) {
    const byte = token[index]
    if (byte !== 0x5c || form.escapePolicy === 'Raw') {
      if (form.delimiterWidth === 3 && byte === 0x0d) {
        if (token[index + 1] !== 0x0a)
          return invalid('isolated carriage return in multiline literal', index)
        output.push(0x0a)
        index += 2
        continue
      }
      if (byte !== undefined) output.push(byte)
      index += 1
      continue
    }
    const escapeOffset = index
    const escaped = Escape.decodeAt(token, index + 1, end, form.delimiter, byteString)
    if (escaped._tag === 'Invalid') return invalid(escaped.detail, escapeOffset)
    output.push(...escaped.bytes)
    index = escaped.next
  }
  if (byteString) return decoded('Bytes', output, false)
  try {
    new TextDecoder('utf-8', { fatal: true }).decode(Uint8Array.from(output))
  } catch {
    return invalid(
      `${form.category === 'Character' ? 'character' : 'text'} literal is not valid UTF-8`,
      contentStart,
    )
  }
  return decoded('Text', output, true)
}

/** One Unicode scalar value decoded from one complete character-literal token. */
export type ScalarResult =
  | { readonly _tag: 'Scalar'; readonly value: number }
  | { readonly _tag: 'Invalid'; readonly detail: string; readonly offset: number }

/**
 * Decodes one character literal to the single Unicode scalar value it denotes.
 *
 * The escape vocabulary is the shared decoder's, so `\n`, `\u{2603}`, and the rest keep exactly
 * one meaning across every literal form. Only the one-scalar rule is decided here, and it is a
 * scalar rule rather than a byte rule: `'é'` decodes to two bytes and one scalar.
 */
export const decodeScalar = (
  token: ReadonlyArray<number>,
  form: LiteralForm.LiteralForm,
): ScalarResult => {
  const result = decode(token, form)
  if (result._tag === 'Invalid') return result
  const text = new TextDecoder('utf-8').decode(Uint8Array.from(result.data.bytes))
  const value = text.codePointAt(0)
  if (value === undefined || String.fromCodePoint(value).length !== text.length)
    return invalid('character literal must hold exactly one Unicode scalar', form.delimiterWidth)
  return Object.freeze({ _tag: 'Scalar', value })
}
