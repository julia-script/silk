import { describe, expect, it } from 'vitest'
import { cursorStateFor, spanLabel } from '../src/InspectorRow.js'

const m = (start: number, end: number) => ({ module: 'main', start, end })

describe('cursorStateFor', () => {
  it('marks the row whose span is exactly the cursor', () => {
    expect(cursorStateFor(m(7, 15), m(7, 15))).toBe('exact')
  })

  it('marks an enclosing row as containing, which is what links phases', () => {
    // The token `identity` sits inside the CallExpression that consumed it, and inside the
    // function that lowered it: one click has to light up all three.
    expect(cursorStateFor(m(0, 51), m(7, 15))).toBe('contains')
  })

  it('leaves disjoint and merely overlapping rows unmarked', () => {
    expect(cursorStateFor(m(52, 106), m(7, 15))).toBeUndefined()
    // Straddles the cursor's start but ends inside it — not an enclosing construct.
    expect(cursorStateFor(m(0, 10), m(7, 15))).toBeUndefined()
  })

  it('has no state without a cursor or without a span', () => {
    expect(cursorStateFor(m(7, 15), undefined)).toBeUndefined()
    expect(cursorStateFor(undefined, m(7, 15))).toBeUndefined()
  })

  it('treats a zero-width recovery span as exact when the cursor sits on it', () => {
    // Recovery inserts zero-width tokens; they must still be selectable.
    expect(cursorStateFor(m(74, 74), m(74, 74))).toBe('exact')
  })

  it('never tints across modules, even for identical byte ranges', () => {
    // The same offsets in another file are a different construct; a bare-offset comparison
    // would light up unrelated code in every imported module.
    expect(cursorStateFor({ module: 'other', start: 7, end: 15 }, m(7, 15))).toBeUndefined()
    expect(cursorStateFor({ module: 'other', start: 0, end: 51 }, m(7, 15))).toBeUndefined()
  })
})

describe('spanLabel', () => {
  it('renders a byte range, and nothing at all when there is no span', () => {
    expect(spanLabel(m(74, 104))).toBe('74–104')
    expect(spanLabel(undefined)).toBe('')
  })
})
