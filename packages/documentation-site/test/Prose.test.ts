import { assert, it } from '@effect/vitest'
import * as Prose from '../src/Prose.js'

const text = (value: string) => ({ _tag: 'Text', value })

it('escapes documentation text rather than emitting it as markup', () => {
  const rendered = Prose.document([
    { _tag: 'Paragraph', children: [text('Wraps <script>alert("x")</script> & Vector<T>.')] },
  ])
  assert.strictEqual(
    rendered,
    '<p>Wraps &lt;script&gt;alert(&quot;x&quot;)&lt;/script&gt; &amp; Vector&lt;T&gt;.</p>',
  )
  assert.notInclude(rendered, '<script>')
})

it('escapes a code block and its language', () => {
  const rendered = Prose.document([
    { _tag: 'CodeBlock', language: 'silk', value: 'let a = b < c && d > e' },
  ])
  assert.strictEqual(
    rendered,
    '<pre><code class="language-silk">let a = b &lt; c &amp;&amp; d &gt; e</code></pre>',
  )
})

it('renders the block kinds the emitter writes', () => {
  assert.strictEqual(
    Prose.document([
      { _tag: 'Heading', depth: 1, children: [text('Examples')] },
      { _tag: 'BlockQuote', children: [{ _tag: 'Paragraph', children: [text('quoted')] }] },
      {
        _tag: 'List',
        ordered: true,
        start: 3,
        items: [[{ _tag: 'Paragraph', children: [text('first')] }]],
      },
      { _tag: 'ThematicBreak' },
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
      { _tag: 'InlineCode', value: 'code' },
      { _tag: 'Emphasis', children: [text('em')] },
      { _tag: 'Strong', children: [text('strong')] },
      { _tag: 'Link', destination: 'https://example.test/?a=1&b=2', children: [text('link')] },
      { _tag: 'Break' },
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
          target: { module: 'silk/option', name: 'unwrapOr' },
        },
        { _tag: 'SymbolLink', spelling: 'missing' },
        {
          _tag: 'SymbolLink',
          spelling: 'elsewhere',
          target: { module: 'silk/other', name: 'elsewhere' },
        },
      ],
      links,
    ),
    '<a class="symbol" href="silk-option.html#unwrapor"><code>unwrapOr</code></a><code>missing</code><code>elsewhere</code>',
  )
})

/**
 * `silk-documentation-json` charters the bootstrap schema as experimental and free to change
 * without migration. A renderer that threw on an unfamiliar tag would turn every schema experiment
 * into a site that does not build, so an unknown node degrades to whatever text it carries.
 */
it('renders an unrecognized node as its recoverable text and keeps going', () => {
  assert.strictEqual(
    Prose.document([
      { _tag: 'Paragraph', children: [text('before')] },
      { _tag: 'Table', children: [{ _tag: 'Paragraph', children: [text('inside a new node')] }] },
      { _tag: 'Footnote', value: 'a leaf added later' },
      { _tag: 'Empty' },
      { _tag: 'Paragraph', children: [text('after')] },
    ]),
    [
      '<p>before</p>',
      '<p>inside a new node</p>',
      '<p>a leaf added later</p>',
      '',
      '<p>after</p>',
    ].join('\n'),
  )
})

it('renders an unrecognized inline node as its recoverable text', () => {
  assert.strictEqual(
    Prose.inline([
      { _tag: 'Strikethrough', children: [text('struck')] },
      { _tag: 'Footnote', value: 'noted' },
      { _tag: 'Nothing' },
    ]),
    'strucknoted',
  )
})
