import type { BlockContent, Definition, PhrasingContent, RootContent } from 'mdast'
import { fromMarkdown } from 'mdast-util-from-markdown'
import * as SilkCodeMirror from './CodeMirror.js'

const safeLink = (destination: string): string | undefined => {
  const normalized = Array.from(destination, (character) => {
    const code = character.charCodeAt(0)
    return code <= 0x1f || code === 0x7f ? '' : character
  }).join('')
  return /^(?:https?|mailto):/i.test(normalized) ? normalized : undefined
}

interface RenderContext {
  readonly definitions: ReadonlyMap<string, Definition>
}

const collectDefinitions = (
  nodes: ReadonlyArray<RootContent | BlockContent>,
  definitions: Map<string, Definition>,
): void => {
  for (const node of nodes) {
    switch (node.type) {
      case 'definition':
        if (!definitions.has(node.identifier)) definitions.set(node.identifier, node)
        break
      case 'blockquote':
        collectDefinitions(node.children, definitions)
        break
      case 'list':
        for (const item of node.children) collectDefinitions(item.children, definitions)
        break
    }
  }
}

const context = (nodes: ReadonlyArray<RootContent>): RenderContext => {
  const definitions = new Map<string, Definition>()
  collectDefinitions(nodes, definitions)
  return { definitions }
}

const element = (tag: string, children: Iterable<Node | string>): HTMLElement => {
  const node = document.createElement(tag)
  node.append(...children)
  return node
}

const anchor = (
  href: string,
  title: string | null | undefined,
  children: Iterable<Node | string>,
): HTMLAnchorElement => {
  const node = document.createElement('a')
  node.href = href
  if (title !== null && title !== undefined) node.title = title
  node.rel = 'noreferrer'
  node.append(...children)
  return node
}

const inlines = (
  context: RenderContext,
  children: ReadonlyArray<PhrasingContent>,
): ReadonlyArray<Node | string> => children.flatMap((child) => inline(context, child))

const inline = (context: RenderContext, node: PhrasingContent): ReadonlyArray<Node | string> => {
  switch (node.type) {
    case 'text':
      return [node.value]
    case 'inlineCode':
      return [element('code', [node.value])]
    case 'emphasis':
      return [element('em', inlines(context, node.children))]
    case 'strong':
      return [element('strong', inlines(context, node.children))]
    case 'link': {
      const href = safeLink(node.url)
      return href === undefined
        ? inlines(context, node.children)
        : [anchor(href, node.title, inlines(context, node.children))]
    }
    case 'linkReference': {
      const definition = context.definitions.get(node.identifier)
      const href = definition === undefined ? undefined : safeLink(definition.url)
      return href === undefined
        ? inlines(context, node.children)
        : [anchor(href, definition?.title, inlines(context, node.children))]
    }
    case 'break':
      return [document.createElement('br')]
    case 'image': {
      const href = safeLink(node.url)
      if (href === undefined) return [node.alt ?? '']
      return [anchor(href, node.title, [node.alt ?? ''])]
    }
    case 'imageReference': {
      const definition = context.definitions.get(node.identifier)
      const href = definition === undefined ? undefined : safeLink(definition.url)
      return href === undefined
        ? [node.alt ?? '']
        : [anchor(href, definition?.title, [node.alt ?? ''])]
    }
    case 'html':
      return [node.value]
    default:
      return []
  }
}

const silk = (value: string): ReadonlyArray<Node | string> => {
  const result: Array<Node | string> = []
  let offset = 0
  for (const range of SilkCodeMirror.highlightRanges(value)) {
    if (range.from < offset || range.to <= range.from) continue
    if (range.from > offset) result.push(value.slice(offset, range.from))
    const span = document.createElement('span')
    span.className = `cm-silk-${range.category}`
    span.textContent = value.slice(range.from, range.to)
    result.push(span)
    offset = range.to
  }
  if (offset < value.length) result.push(value.slice(offset))
  return result
}

const block = (context: RenderContext, node: RootContent | BlockContent): Node | undefined => {
  switch (node.type) {
    case 'paragraph':
      return element('p', inlines(context, node.children))
    case 'heading':
      return element(`h${node.depth}`, inlines(context, node.children))
    case 'code': {
      const pre = document.createElement('pre')
      if (node.lang !== null && node.lang !== undefined) pre.dataset.language = node.lang
      pre.append(
        element('code', node.lang?.toLowerCase() === 'silk' ? silk(node.value) : [node.value]),
      )
      return pre
    }
    case 'blockquote':
      return element('blockquote', blocks(context, node.children))
    case 'list': {
      const list = document.createElement(node.ordered === true ? 'ol' : 'ul')
      if (node.ordered === true && node.start !== null && node.start !== undefined)
        list.setAttribute('start', String(node.start))
      for (const item of node.children) list.append(element('li', blocks(context, item.children)))
      return list
    }
    case 'thematicBreak':
      return document.createElement('hr')
    case 'html':
      return element('p', [node.value])
    case 'definition':
      return undefined
    default:
      return undefined
  }
}

const blocks = (
  context: RenderContext,
  nodes: ReadonlyArray<RootContent | BlockContent>,
): ReadonlyArray<Node> =>
  nodes.flatMap((node) => {
    const rendered = block(context, node)
    return rendered === undefined ? [] : [rendered]
  })

/**
 * Renders a language-server hover's CommonMark payload into a detached DOM node.
 *
 * **When to use**
 *
 * Use when a browser host needs the editor's canonical, framework-free hover rendering.
 *
 * **Details**
 *
 * Links are emitted only for explicit `http:`, `https:`, and `mailto:` destinations after
 * CommonMark entity decoding and ASCII control-character removal. Other destinations and raw HTML
 * render as text. Fenced `silk` code receives the same lexer-driven classes as the editor.
 *
 * **Gotchas**
 *
 * Rendering requires a DOM `document` global. The returned node is detached, and this module does
 * not inject CSS; the host owns styles for `cm-silk-type-tooltip` and the emitted highlight classes.
 *
 * @category converting
 * @since 0.0.0
 */
export const render = (markdown: string): HTMLElement => {
  const rootContent = fromMarkdown(markdown).children
  const root = document.createElement('div')
  root.className = 'cm-silk-type-tooltip'
  root.append(...blocks(context(rootContent), rootContent))
  return root
}
