import { assert, it } from '@effect/vitest'
import * as ByteString from '../src/ByteString.js'

it('copies input bytes and compares by byte content', () => {
  const input = Uint8Array.of(0x61, 0x62)
  const value = ByteString.fromUint8Array(input)
  input[0] = 0xff

  assert.deepEqual(ByteString.toUint8Array(value), Uint8Array.of(0x61, 0x62))
  assert.strictEqual(ByteString.equals(value, ByteString.fromString('ab')), true)
  assert.strictEqual(ByteString.hash(value), ByteString.hash(ByteString.fromString('ab')))
})

it('encodes UTF-8 and applies LLVM byte escaping', () => {
  const value = ByteString.fromString('a"\\\nλ')
  assert.deepEqual(
    ByteString.toUint8Array(value),
    Uint8Array.of(0x61, 0x22, 0x5c, 0x0a, 0xce, 0xbb),
  )
  assert.strictEqual(ByteString.escapeForIr(value), 'a\\22\\5C\\0A\\CE\\BB')
})
