import * as Html from './Html.js'

/**
 * Renders the documentation JSON's block and inline trees as HTML.
 *
 * The walk is defensive on purpose. `silk-documentation-json` charters the bootstrap schema as
 * experimental and free to change without migration, so a node tag this renderer has never seen is
 * a normal event rather than a broken file. An unrecognized node renders as whatever text it can
 * recover — its own `value`, or its children — and the rest of the page is unaffected. Refusing it
 * would turn every schema experiment into a site that does not build.
 */

/**
 * Resolves a declaration the emitter linked to into a page location.
 *
 * Prose does not know the site's file layout, and the layout settles slug collisions in module
 * order, so the mapping is supplied rather than recomputed. An unresolved link stays inline code,
 * which is what the emitter already does for a symbol it could not resolve.
 */
export type Links = (module: string, name: string) => string | undefined

/** Resolves nothing. Every symbol link renders as inline code. */
export const noLinks: Links = () => undefined

const isRecord = (value: unknown): value is Record<string, unknown> =>
  typeof value === 'object' && value !== null && !Array.isArray(value)

const textAt = (value: Record<string, unknown>, key: string): string =>
  typeof value[key] === 'string' ? value[key] : ''

const childrenOf = (value: Record<string, unknown>): ReadonlyArray<unknown> =>
  Array.isArray(value.children) ? value.children : []

const symbolLink = (node: Record<string, unknown>, links: Links): string => {
  const spelling = `<code>${Html.escapeText(textAt(node, 'spelling'))}</code>`
  const target = node.target
  if (!isRecord(target)) return spelling
  const location = links(textAt(target, 'module'), textAt(target, 'name'))
  return location === undefined
    ? spelling
    : `<a class="symbol" href="${Html.escapeText(location)}">${spelling}</a>`
}

/** Renders one inline node, or its recoverable text when the tag is unfamiliar. */
export const inline = (node: unknown, links: Links = noLinks): string => {
  if (Array.isArray(node)) return node.map((child) => inline(child, links)).join('')
  if (!isRecord(node)) return ''
  const children = (): string => inline(childrenOf(node), links)
  switch (node._tag) {
    case 'Text':
      return Html.escapeText(textAt(node, 'value'))
    case 'InlineCode':
      return `<code>${Html.escapeText(textAt(node, 'value'))}</code>`
    case 'Emphasis':
      return `<em>${children()}</em>`
    case 'Strong':
      return `<strong>${children()}</strong>`
    case 'Link':
      return `<a href="${Html.escapeText(textAt(node, 'destination'))}">${children()}</a>`
    case 'SymbolLink':
      return symbolLink(node, links)
    case 'Break':
      return '<br>'
    default:
      return childrenOf(node).length > 0 ? children() : Html.escapeText(textAt(node, 'value'))
  }
}

/** Renders one block node, or its recoverable text when the tag is unfamiliar. */
export const block = (node: unknown, links: Links = noLinks): string => {
  if (Array.isArray(node)) return node.map((child) => block(child, links)).join('')
  if (!isRecord(node)) return ''
  switch (node._tag) {
    case 'Paragraph':
      return `<p>${inline(childrenOf(node), links)}</p>`
    case 'Heading': {
      const depth = typeof node.depth === 'number' ? node.depth : 1
      // A documentation heading sits inside a declaration's section, so it starts below the page's
      // own headings rather than competing with them.
      const level = Math.min(6, Math.max(1, Math.trunc(depth)) + 2)
      return `<h${level}>${inline(childrenOf(node), links)}</h${level}>`
    }
    case 'CodeBlock': {
      const language = textAt(node, 'language')
      const attribute =
        language === '' ? '' : ` class="language-${Html.escapeText(Html.slug(language))}"`
      return `<pre><code${attribute}>${Html.escapeText(textAt(node, 'value'))}</code></pre>`
    }
    case 'BlockQuote':
      return `<blockquote>${block(childrenOf(node), links)}</blockquote>`
    case 'List': {
      const ordered = node.ordered === true
      const start = node.start
      const attribute =
        ordered && typeof start === 'number' && start !== 1
          ? ` start="${Html.escapeText(String(start))}"`
          : ''
      const items = (Array.isArray(node.items) ? node.items : [])
        .map((item) => `<li>${block(item, links)}</li>`)
        .join('')
      return ordered ? `<ol${attribute}>${items}</ol>` : `<ul>${items}</ul>`
    }
    case 'ThematicBreak':
      return '<hr>'
    default: {
      const recovered = childrenOf(node).length > 0 ? block(childrenOf(node), links) : ''
      if (recovered !== '') return recovered
      const text = textAt(node, 'value')
      return text === '' ? '' : `<p>${Html.escapeText(text)}</p>`
    }
  }
}

/** Renders a whole document body. */
export const document = (
  blocks: ReadonlyArray<unknown> | undefined,
  links: Links = noLinks,
): string => (blocks === undefined ? '' : blocks.map((node) => block(node, links)).join('\n'))
