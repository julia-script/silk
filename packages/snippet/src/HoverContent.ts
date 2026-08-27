/**
 * Renders one language-server hover's CommonMark payload as plain DOM.
 *
 * Hover content is compiler-authored but flows through Markdown, so links are restricted to
 * http(s) and mailto and every other scheme renders as plain text. Nested `silk` code blocks are
 * highlighted with the same lexer-driven classification the editor uses, so hover code and editor
 * code never disagree.
 */

import * as SilkCodeMirror from '@silk-lang/language/CodeMirror'
import type { BlockContent, PhrasingContent, RootContent } from 'mdast'
import { fromMarkdown } from 'mdast-util-from-markdown'

const safeLink = (destination: string): string | undefined => {
  const normalized = destination.trim()
  if (normalized.startsWith('//')) return undefined
  const scheme = /^([a-z][a-z\d+.-]*):/i.exec(normalized)?.[1]?.toLowerCase()
  return scheme === undefined || scheme === 'http' || scheme === 'https' || scheme === 'mailto'
    ? destination
    : undefined
}

const element = (tag: string, children: Iterable<Node | string>): HTMLElement => {
  const node = document.createElement(tag)
  node.append(...children)
  return node
}

const inlines = (children: ReadonlyArray<PhrasingContent>): ReadonlyArray<Node | string> =>
  children.flatMap((child) => inline(child))

const inline = (node: PhrasingContent): ReadonlyArray<Node | string> => {
  switch (node.type) {
    case 'text':
      return [node.value]
    case 'inlineCode':
      return [element('code', [node.value])]
    case 'emphasis':
      return [element('em', inlines(node.children))]
    case 'strong':
      return [element('strong', inlines(node.children))]
    case 'link': {
      const href = safeLink(node.url)
      if (href === undefined) return inlines(node.children)
      const anchor = document.createElement('a')
      anchor.href = href
      if (node.title !== null && node.title !== undefined) anchor.title = node.title
      anchor.rel = 'noreferrer'
      anchor.append(...inlines(node.children))
      return [anchor]
    }
    case 'linkReference':
      return inlines(node.children)
    case 'break':
      return [document.createElement('br')]
    case 'image': {
      const href = safeLink(node.url)
      if (href === undefined) return [node.alt ?? '']
      const anchor = document.createElement('a')
      anchor.href = href
      if (node.title !== null && node.title !== undefined) anchor.title = node.title
      anchor.rel = 'noreferrer'
      anchor.append(node.alt ?? '')
      return [anchor]
    }
    case 'imageReference':
      return [node.alt ?? '']
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

const block = (node: RootContent | BlockContent): Node | undefined => {
  switch (node.type) {
    case 'paragraph':
      return element('p', inlines(node.children))
    case 'heading':
      return element(`h${node.depth}`, inlines(node.children))
    case 'code': {
      const pre = document.createElement('pre')
      if (node.lang !== null && node.lang !== undefined) pre.dataset['language'] = node.lang
      pre.append(
        element('code', node.lang?.toLowerCase() === 'silk' ? silk(node.value) : [node.value]),
      )
      return pre
    }
    case 'blockquote':
      return element('blockquote', blocks(node.children))
    case 'list': {
      const list = document.createElement(node.ordered === true ? 'ol' : 'ul')
      if (node.ordered === true && node.start !== null && node.start !== undefined)
        list.setAttribute('start', String(node.start))
      for (const item of node.children) list.append(element('li', blocks(item.children)))
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

const blocks = (nodes: ReadonlyArray<RootContent | BlockContent>): ReadonlyArray<Node> =>
  nodes.flatMap((node) => {
    const rendered = block(node)
    return rendered === undefined ? [] : [rendered]
  })

/** Renders the CommonMark payload of one language-server hover into a detached tooltip node. */
export const render = (markdown: string): HTMLElement => {
  const root = document.createElement('div')
  root.className = 'cm-silk-type-tooltip'
  root.append(...blocks(fromMarkdown(markdown).children))
  return root
}
