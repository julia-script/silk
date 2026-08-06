import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { cursorStateFor, RowList, spanLabel } from './row'

describe('cursorStateFor', () => {
  it('marks the row whose span is exactly the cursor', () => {
    expect(cursorStateFor({ start: 7, end: 15 }, { start: 7, end: 15 })).toBe('exact')
  })

  it('marks an enclosing row as containing, which is what links phases', () => {
    // The token `identity` sits inside the CallExpression that consumed it, and inside the
    // function that lowered it: one click has to light up all three.
    expect(cursorStateFor({ start: 0, end: 51 }, { start: 7, end: 15 })).toBe('contains')
  })

  it('leaves disjoint and merely overlapping rows unmarked', () => {
    expect(cursorStateFor({ start: 52, end: 106 }, { start: 7, end: 15 })).toBeUndefined()
    // Straddles the cursor's start but ends inside it — not an enclosing construct.
    expect(cursorStateFor({ start: 0, end: 10 }, { start: 7, end: 15 })).toBeUndefined()
  })

  it('has no state without a cursor or without a span', () => {
    expect(cursorStateFor({ start: 7, end: 15 }, undefined)).toBeUndefined()
    expect(cursorStateFor(undefined, { start: 7, end: 15 })).toBeUndefined()
  })

  it('treats a zero-width recovery span as exact when the cursor sits on it', () => {
    // Recovery inserts zero-width tokens; they must still be selectable.
    expect(cursorStateFor({ start: 74, end: 74 }, { start: 74, end: 74 })).toBe('exact')
  })
})

describe('spanLabel', () => {
  it('renders a byte range, and nothing at all when there is no span', () => {
    expect(spanLabel({ start: 74, end: 104 })).toBe('74–104')
    expect(spanLabel(undefined)).toBe('')
  })
})

describe('RowList', () => {
  it('renders rows with a cursor state and marks the exact row as current', () => {
    const markup = renderToStaticMarkup(
      <RowList
        label="Test rows"
        cursor={{ start: 7, end: 15 }}
        rows={[
          { key: 'a', label: 'SourceFile', span: { start: 0, end: 106 } },
          { key: 'b', label: 'Identifier', span: { start: 7, end: 15 } },
          { key: 'c', label: 'Elsewhere', span: { start: 52, end: 106 } },
        ]}
      />,
    )

    expect(markup).toContain('data-cursor="contains"')
    expect(markup).toContain('data-cursor="exact"')
    expect(markup).toContain('aria-current="true"')
    expect(markup).toContain('0–106')
  })

  it('renders a row that can move the cursor as a button, and a static row as not', () => {
    const markup = renderToStaticMarkup(
      <RowList
        label="Test rows"
        rows={[
          { key: 'a', label: 'clickable', onActivate: () => undefined },
          { key: 'b', label: 'static' },
        ]}
      />,
    )

    expect(markup).toContain('data-pickable="true"')
    expect(markup).toContain('data-pickable="false"')
    expect(markup).toContain('<button')
  })
})
