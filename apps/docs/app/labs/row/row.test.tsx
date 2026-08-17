import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { RowList } from './row'

const span = (start: number, end: number) => ({ module: 'main', start, end })

describe('RowList', () => {
  it('renders rows with a cursor state and marks the exact row as current', () => {
    const markup = renderToStaticMarkup(
      <RowList
        label="Test rows"
        cursor={span(7, 15)}
        rows={[
          { key: 'a', label: 'SourceFile', span: span(0, 106) },
          { key: 'b', label: 'Identifier', span: span(7, 15) },
          { key: 'c', label: 'Elsewhere', span: span(52, 106) },
        ]}
      />,
    )

    expect(markup).toContain('data-cursor="contains"')
    expect(markup).toContain('data-cursor="exact"')
    expect(markup).toContain('aria-current="true"')
    expect(markup).toContain('0–106')
  })

  it('renders a row with a span as a button when picking is wired, and a static row as not', () => {
    const markup = renderToStaticMarkup(
      <RowList
        label="Test rows"
        onPick={() => undefined}
        rows={[
          { key: 'a', label: 'clickable', span: span(0, 5) },
          { key: 'b', label: 'static' },
        ]}
      />,
    )

    expect(markup).toContain('data-pickable="true"')
    expect(markup).toContain('data-pickable="false"')
    expect(markup).toContain('<button')
  })

  it('never tints a row whose span lives in another module', () => {
    const markup = renderToStaticMarkup(
      <RowList
        label="Test rows"
        cursor={span(7, 15)}
        rows={[{ key: 'a', label: 'Other', span: { module: 'other', start: 7, end: 15 } }]}
      />,
    )

    expect(markup).not.toContain('data-cursor="exact"')
    expect(markup).not.toContain('aria-current')
  })
})
