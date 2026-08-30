import { assert, it } from '@effect/vitest'
import * as ByteString from '../src/ByteString.js'

const utf8Vectors = [
  { label: 'lone high surrogate', value: '\uD800' },
  { label: 'lone low surrogate', value: '\uDC00' },
  { label: 'adjacent malformed units', value: '\uDC00\uD800' },
  { label: 'mixed valid and malformed text', value: 'A\uD800λ\uDC00Z' },
  { label: 'BMP text', value: 'AλЖZ' },
  { label: 'supplementary pair', value: 'A😀Z' },
] as const

it('copies input bytes and compares by byte content', () => {
  const input = Uint8Array.of(0x61, 0x62)
  const value = ByteString.fromUint8Array(input)
  input[0] = 0xff

  assert.deepEqual(ByteString.toUint8Array(value), Uint8Array.of(0x61, 0x62))
  assert.strictEqual(ByteString.equals(value, ByteString.fromString('ab')), true)

  const arbitrary = Uint8Array.of(0xff, 0xed, 0xa0, 0x80)
  const exact = ByteString.fromUint8Array(arbitrary)
  arbitrary.fill(0)
  assert.deepEqual(ByteString.toUint8Array(exact), Uint8Array.of(0xff, 0xed, 0xa0, 0x80))
})

it('encodes UTF-8 and applies LLVM byte escaping', () => {
  const value = ByteString.fromString('a"\\\nλ')
  assert.deepEqual(
    ByteString.toUint8Array(value),
    Uint8Array.of(0x61, 0x22, 0x5c, 0x0a, 0xce, 0xbb),
  )
  assert.strictEqual(ByteString.escapeForIr(value), 'a\\22\\5C\\0A\\CE\\BB')
})

it('matches the TextEncoder scalar-value policy for malformed JavaScript strings', () => {
  const encoder = new TextEncoder()
  const decoder = new TextDecoder('utf-8', { fatal: true })

  for (const vector of utf8Vectors) {
    const actual = ByteString.toUint8Array(ByteString.fromString(vector.value))
    const expected = encoder.encode(vector.value)
    assert.deepEqual(actual, expected, vector.label)
    assert.strictEqual(decoder.decode(actual), decoder.decode(expected), vector.label)
  }
})
