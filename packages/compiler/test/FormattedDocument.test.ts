import { assert, it } from '@effect/vitest'
import * as FormattedDocument from '../src/FormattedDocument.js'

it('owns immutable bytes and returns defensive adapter copies', () => {
  const input = Uint8Array.of(0x61, 0x0a)
  const document = FormattedDocument.make(input, true)
  input[0] = 0x62

  assert.strictEqual(Object.isFrozen(document), true)
  assert.strictEqual(Object.isFrozen(document.bytes), true)
  assert.deepEqual(document.bytes, [0x61, 0x0a])
  assert.strictEqual(document.changed, true)

  const copy = FormattedDocument.toUint8Array(document)
  copy[0] = 0x63
  assert.deepEqual(document.bytes, [0x61, 0x0a])
})
