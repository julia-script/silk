import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import * as Hover from './Hover'

describe('Content', () => {
  it('renders a documented Silk hover as highlighted code followed by Markdown prose', () => {
    const markup = renderToStaticMarkup(
      <Hover.Content
        markdown={`\`\`\`silk
pub struct OutOfMemory
\`\`\`

Allocates **owned storage** through an explicitly provided implementation.`}
      />,
    )

    expect(markup).not.toContain('```')
    expect(markup).toContain('<pre')
    expect(markup).toContain('cm-silk-keyword')
    expect(markup).toContain('<p>Allocates <strong>owned storage</strong>')
  })

  it('renders safe links and does not make executable Markdown links interactive', () => {
    const markup = renderToStaticMarkup(
      <Hover.Content markdown="Read [the guide](https://example.com) or [do not](javascript:alert(1))." />,
    )

    expect(markup).toContain('href="https://example.com"')
    expect(markup).not.toContain('href="javascript:')
  })
})
