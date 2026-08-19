import { assert, it } from '@effect/vitest'
import type * as Model from '../src/Model.js'
import * as Prose from '../src/Prose.js'

const source = Object.freeze({ sourceId: 'docs/main', start: 0, end: 10 })
const text = (value: string): Model.Inline => Object.freeze({ _tag: 'Text', value, source })

it('escapes documentation text rather than emitting it as markup', () => {
  const rendered = Prose.document([
    {
      _tag: 'Paragraph',
      children: [text('Wraps <script>alert("x")</script> & Vector<T>.')],
      source,
    },
  ])
  assert.strictEqual(
    rendered,
    '<p>Wraps &lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt; &amp; Vector&lt;T&gt;.</p>',
  )
  assert.notInclude(rendered, '<script>')
})

it('escapes a code block and its language', () => {
  const rendered = Prose.document([
    {
      _tag: 'CodeBlock',
      language: 'silk',
      value: 'let a = b < c && d > e',
      example: false,
      source,
    },
  ])
  assert.strictEqual(
    rendered,
    '<pre><code class="language-silk">let a = b &lt; c &amp;&amp; d &gt; e</code></pre>',
  )
})

it('renders the block kinds the emitter writes', () => {
  assert.strictEqual(
    Prose.document([
      { _tag: 'Heading', depth: 1, children: [text('Examples')], source },
      {
        _tag: 'BlockQuote',
        children: [{ _tag: 'Paragraph', children: [text('quoted')], source }],
        source,
      },
      {
        _tag: 'List',
        ordered: true,
        start: 3,
        items: [[{ _tag: 'Paragraph', children: [text('first')], source }]],
        source,
      },
      { _tag: 'ThematicBreak', source },
    ]),
    [
      '<h3>Examples</h3>',
      '<blockquote><p>quoted</p></blockquote>',
      '<ol start="3"><li><p>first</p></li></ol>',
      '<hr>',
    ].join('\n'),
  )
})

it('renders the inline kinds the emitter writes', () => {
  assert.strictEqual(
    Prose.inline([
      text('a '),
      { _tag: 'InlineCode', value: 'code', source },
      { _tag: 'Emphasis', children: [text('em')], source },
      { _tag: 'Strong', children: [text('strong')], source },
      {
        _tag: 'Link',
        destination: 'https://example.test/?a=1&b=2',
        children: [text('link')],
        source,
      },
      { _tag: 'Break', source },
    ]),
    'a <code>code</code><em>em</em><strong>strong</strong><a href="https://example.test/?a=1&amp;b=2">link</a><br>',
  )
})

/**
 * The emitter resolves a `[`Name`]` link to a target when it can and leaves it unresolved when it
 * cannot. Prose does not know the site's layout, so it asks — and an unanswered link stays inline
 * code exactly as an unresolved one does.
 */
it('links a resolved symbol and leaves an unresolved one as code', () => {
  const links: Prose.Links = (module, name) =>
    module === 'silk/option' && name === 'unwrapOr' ? 'silk-option.html#unwrapor' : undefined
  assert.strictEqual(
    Prose.inline(
      [
        {
          _tag: 'SymbolLink',
          spelling: 'unwrapOr',
          target: {
            id: 'silk/option::unwrapOr',
            module: 'silk/option',
            name: 'unwrapOr',
            kind: 'Function',
          },
          source,
        },
        { _tag: 'SymbolLink', spelling: 'missing', source },
        {
          _tag: 'SymbolLink',
          spelling: 'elsewhere',
          target: {
            id: 'silk/other::elsewhere',
            module: 'silk/other',
            name: 'elsewhere',
            kind: 'Function',
          },
          source,
        },
      ],
      links,
    ),
    '<a class="symbol" href="silk-option.html#unwrapor"><code>unwrapOr</code></a><code>missing</code><code>elsewhere</code>',
  )
})
