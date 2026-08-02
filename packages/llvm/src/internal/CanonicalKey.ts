import type * as ByteString from '../ByteString.js'

export type CanonicalKey = string

/** @internal */
export const bytes = (value: ByteString.ByteString): CanonicalKey => {
  let hexadecimal = ''
  for (const byte of value.bytes) hexadecimal += byte.toString(16).padStart(2, '0')
  return `${value.bytes.length}:${hexadecimal}`
}

/** @internal */
export const integer = (value: number | bigint): CanonicalKey =>
  typeof value === 'bigint' ? `i${value.toString()}` : `n${value.toString()}`

/** @internal */
export const sequence = (values: Iterable<CanonicalKey>): CanonicalKey => {
  let result = ''
  for (const value of values) result += `${value.length}:${value}`
  return result
}

/** @internal */
export const tagged = (tag: string, values: Iterable<CanonicalKey>): CanonicalKey =>
  `${tag.length}:${tag}${sequence(values)}`
