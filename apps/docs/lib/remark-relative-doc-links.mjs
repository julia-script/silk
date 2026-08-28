/**
 * Make extension-bearing Markdown links explicit relative paths for Fumadocs.
 *
 * Bare links such as `values-and-types.md` are valid Markdown and work when the source is read on
 * GitHub. Fumadocs resolves only links beginning with `./` or `../`, however, so it otherwise emits
 * the source URL unchanged and the deployed site requests a nonexistent `.md` route.
 */
export function remarkRelativeDocLinks() {
  return (tree) => {
    visit(tree, (node) => {
      if (node.type !== 'link' || typeof node.url !== 'string') return;
      if (!isBareMarkdownPath(node.url)) return;

      node.url = `./${node.url}`;
    });
  };
}

function isBareMarkdownPath(url) {
  if (url.startsWith('/') || url.startsWith('./') || url.startsWith('../')) return false;
  if (/^[A-Za-z][A-Za-z\d+.-]*:/.test(url)) return false;

  const path = url.split(/[?#]/, 1)[0];
  return path.endsWith('.md') || path.endsWith('.mdx');
}

function visit(node, callback) {
  callback(node);
  for (const child of node.children ?? []) visit(child, callback);
}
