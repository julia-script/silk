/**
 * Use a document's leading `# H1` as its Fumadocs title and drop it from the body.
 *
 * Package docs under `packages/*​/docs` are plain Markdown with no frontmatter — they are
 * published to npm and read on GitHub, where frontmatter would render as noise. This lets the
 * site derive titles without every package having to maintain a parallel frontmatter block.
 *
 * Files that *do* declare a frontmatter title keep it, and their H1 is left alone.
 */
export function remarkH1Title() {
  return (tree, file) => {
    const index = tree.children.findIndex((node) => node.type === 'heading' && node.depth === 1)
    if (index === -1) return

    const heading = tree.children[index]
    // Fumadocs renders the title itself via <DocsTitle>, so drop the duplicate heading.
    tree.children.splice(index, 1)

    // The schema fills a file-name fallback and flags it; the H1 is the better title.
    // An author-written frontmatter title always wins.
    const frontmatter = (file.data.frontmatter ??= {})
    if (frontmatter.titleFromFileName) frontmatter.title = toText(heading)
  }
}

function toText(node) {
  if (typeof node.value === 'string') return node.value
  return (node.children ?? []).map(toText).join('')
}
