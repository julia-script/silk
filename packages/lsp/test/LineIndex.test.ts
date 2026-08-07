import { assert, it } from '@effect/vitest'
import * as LineIndex from '../src/LineIndex.js'

const encoder = new TextEncoder()

it('maps ascii offsets to lines and characters', () => {
  const index = LineIndex.make(encoder.encode('ab\ncd\n\nef'))
  assert.deepEqual(LineIndex.positionOf(index, 0), { line: 0, character: 0 })
  assert.deepEqual(LineIndex.positionOf(index, 2), { line: 0, character: 2 })
  assert.deepEqual(LineIndex.positionOf(index, 3), { line: 1, character: 0 })
  assert.deepEqual(LineIndex.positionOf(index, 6), { line: 2, character: 0 })
  assert.deepEqual(LineIndex.positionOf(index, 9), { line: 3, character: 2 })
})

it('counts multi-byte characters as UTF-16 units', () => {
  const index = LineIndex.make(encoder.encode('aπb\n🙂x'))
  assert.deepEqual(LineIndex.positionOf(index, 4), { line: 0, character: 3 })
  assert.deepEqual(LineIndex.positionOf(index, 9), { line: 1, character: 2 })
  assert.strictEqual(LineIndex.offsetOf(index, { line: 0, character: 3 }), 4)
  assert.strictEqual(LineIndex.offsetOf(index, { line: 1, character: 2 }), 9)
})

it('clamps positions outside the document', () => {
  const index = LineIndex.make(encoder.encode('ab'))
  assert.strictEqual(LineIndex.offsetOf(index, { line: 9, character: 0 }), 2)
  assert.strictEqual(LineIndex.offsetOf(index, { line: 0, character: 99 }), 2)
  assert.deepEqual(LineIndex.positionOf(index, 99), { line: 0, character: 2 })
})
