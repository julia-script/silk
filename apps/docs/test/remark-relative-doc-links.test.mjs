import { assert, it } from '@effect/vitest';
import { remarkRelativeDocLinks } from '../lib/remark-relative-doc-links.mjs';

it('marks bare Markdown document links as relative for Fumadocs resolution', () => {
  const tree = {
    type: 'root',
    children: [
      { type: 'link', url: 'values-and-types.md', children: [] },
      { type: 'link', url: 'typed-failures.md#fatal-traps', children: [] },
      { type: 'link', url: 'guides/start.mdx?mode=full#intro', children: [] },
    ],
  };

  remarkRelativeDocLinks()(tree);

  assert.deepStrictEqual(
    tree.children.map(({ url }) => url),
    [
      './values-and-types.md',
      './typed-failures.md#fatal-traps',
      './guides/start.mdx?mode=full#intro',
    ],
  );
});

it('leaves explicit relative, absolute, fragment, and external links unchanged', () => {
  const urls = [
    './values-and-types.md',
    '../language/tutorial.md',
    '/docs/reference',
    '#values',
    'https://example.com/guide.md',
    'mailto:docs@example.com',
  ];
  const tree = {
    type: 'root',
    children: urls.map((url) => ({ type: 'link', url, children: [] })),
  };

  remarkRelativeDocLinks()(tree);

  assert.deepStrictEqual(
    tree.children.map(({ url }) => url),
    urls,
  );
});
