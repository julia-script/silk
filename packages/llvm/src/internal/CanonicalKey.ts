import type * as ByteString from '../ByteString.js'

export type CanonicalKey = string

// Native function declaration encoded each long symbol twice (collision check and insertion).
// Reuse keys for immutable byte arrays and avoid formatting every byte on each first visit.
const byteKeys = new WeakMap<Uint8Array, CanonicalKey>()
// Mangled symbol names average about a kilobyte, and every live name keeps its key. One code
// unit per byte keeps the key a one-byte string the size of the name; hex doubled it.
const chunk = 8192

/** @internal */
export const bytes = (value: ByteString.ByteString): CanonicalKey => {
  const cached = byteKeys.get(value.bytes)
  if (cached !== undefined) return cached
  let latin1 = ''
  for (let offset = 0; offset < value.bytes.length; offset += chunk) {
    latin1 += String.fromCharCode(...value.bytes.subarray(offset, offset + chunk))
  }
  const key = `${value.bytes.length}:${latin1}`
  byteKeys.set(value.bytes, key)
  return key
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
