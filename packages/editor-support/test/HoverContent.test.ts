import { assert, describe, it } from '@effect/vitest'
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

  it('keeps explicit http, https, and mailto links with safe attributes', () => {
    const root = HoverContent.render(
      '[http](http://example.com) [https](https://example.com "Docs") [mail](mailto:hello@example.com) [normalized](h&#10;ttps://normalized.example)',
    )
    const anchors = [...root.querySelectorAll('a')]
    assert.deepEqual(
      anchors.map((anchor) => anchor.getAttribute('href')),
      [
        'http://example.com',
        'https://example.com',
        'mailto:hello@example.com',
        'https://normalized.example',
      ],
    )
    assert.deepEqual(
      anchors.map((anchor) => anchor.getAttribute('rel')),
      ['noreferrer', 'noreferrer', 'noreferrer', 'noreferrer'],
    )
    assert.strictEqual(anchors[1]?.getAttribute('title'), 'Docs')
  })

  it('renders every non-allowlisted destination as plain text after normalization', () => {
    const root = HoverContent.render(
      '[relative](guide) [fragment](#topic) [protocol-relative](//evil.example) [unknown](vscode://open) [encoded-control](java&#10;script:alert(1)) [encoded-space](&#32;https://space.example) [non-ASCII-space](&nbsp;https://space.example).',
    )
    assert.isNull(root.querySelector('a'))
    assert.strictEqual(
      root.textContent,
      'relative fragment protocol-relative unknown encoded-control encoded-space non-ASCII-space.',
    )
  })

  it('applies the same allowlist to CommonMark reference links and images', () => {
    const root = HoverContent.render(
      '[docs][safe] [blocked][unsafe] ![diagram][safe-image] ![blocked image][unsafe-image]\n\n[safe]: https://reference.example "Reference"\n[unsafe]: javascript:alert(1)\n[safe-image]: mailto:diagram@example.com\n[unsafe-image]: #fragment',
    )
    const anchors = [...root.querySelectorAll('a')]
    assert.deepEqual(
      anchors.map((item) => [item.textContent, item.getAttribute('href')]),
      [
        ['docs', 'https://reference.example'],
        ['diagram', 'mailto:diagram@example.com'],
      ],
    )
    assert.strictEqual(anchors[0]?.getAttribute('title'), 'Reference')
    assert.strictEqual(root.textContent, 'docs blocked diagram blocked image')
  })

  it('renders authored HTML literally without admitting authored elements', () => {
    const root = HoverContent.render('<button onclick="alert(1)">press</button>')
    assert.isNull(root.querySelector('button'))
    assert.strictEqual(root.textContent, '<button onclick="alert(1)">press</button>')
  })

  it('renders lists, quotes, and headings structurally', () => {
    const root = HoverContent.render('# Title\n\n> quoted\n\n- one\n- two\n\n---')
    assert.isNotNull(root.querySelector('h1'))
    assert.isNotNull(root.querySelector('blockquote'))
    assert.strictEqual(root.querySelectorAll('li').length, 2)
    assert.isNotNull(root.querySelector('hr'))
  })

  it('returns detached DOM and leaves styling to the host', () => {
    const headBefore = document.head.innerHTML
    const root = HoverContent.render('```silk\npub struct Item\n```')
    assert.isFalse(root.isConnected)
    assert.strictEqual(root.className, 'cm-silk-type-tooltip')
    assert.isNotNull(root.querySelector('.cm-silk-keyword'))
    assert.isNull(root.querySelector('style, link[rel="stylesheet"]'))
    assert.strictEqual(document.head.innerHTML, headBefore)
  })
})
