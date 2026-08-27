import { assert, describe, it } from 'vitest'
import * as HoverContent from '../src/HoverContent.js'

describe('render', () => {
  it('renders a documented Silk hover as highlighted code followed by Markdown prose', () => {
    const root = HoverContent.render(
      '```silk\npub struct OutOfMemoryError\n```\n\nAllocates **owned storage** through an explicitly provided implementation.',
    )
    assert.notInclude(root.innerHTML, '```')
    assert.include(root.innerHTML, '<pre')
    assert.include(root.innerHTML, 'cm-silk-keyword')
    assert.include(root.innerHTML, '<p>Allocates <strong>owned storage</strong>')
  })

  it('keeps http links and strips their formatting risks', () => {
    const root = HoverContent.render('See [the docs](https://example.com "Docs").')
    const anchor = root.querySelector('a')
    assert.isNotNull(anchor)
    assert.strictEqual(anchor?.getAttribute('href'), 'https://example.com')
    assert.strictEqual(anchor?.getAttribute('rel'), 'noreferrer')
  })

  it('renders unsafe link schemes as plain text', () => {
    const root = HoverContent.render(
      'Do not [click](javascript:alert(1)) or [peek](vscode://open) or [drift](//evil.example).',
    )
    assert.isNull(root.querySelector('a'))
    assert.include(root.textContent ?? '', 'click')
    assert.include(root.textContent ?? '', 'peek')
    assert.include(root.textContent ?? '', 'drift')
  })

  it('renders lists, quotes, and headings structurally', () => {
    const root = HoverContent.render('# Title\n\n> quoted\n\n- one\n- two\n\n---')
    assert.isNotNull(root.querySelector('h1'))
    assert.isNotNull(root.querySelector('blockquote'))
    assert.strictEqual(root.querySelectorAll('li').length, 2)
    assert.isNotNull(root.querySelector('hr'))
  })
})
