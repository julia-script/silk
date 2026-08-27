/**
 * Renders the introduction essay's Markdown as the landing page's HTML.
 *
 * Every `silk` fence is compiled at render time with the same convention doctest verifies —
 * one standalone module, default target — and only fences that compile cleanly become live
 * snippets with diagnostics and hover. Fragments stay highlight-only, so the page can never
 * show a false diagnostic, and the live set can never go stale against the compiler.
 */

import * as Analysis from '@silk-effect/compiler/Analysis'
import * as Effect from 'effect/Effect'
import type { BlockContent, PhrasingContent, RootContent } from 'mdast'
import { fromMarkdown } from 'mdast-util-from-markdown'

const encoder = new TextEncoder()

const escapeHtml = (value: string): string =>
  value
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')

const compiles = (code: string, ordinal: number): boolean => {
  const snapshot = Effect.runSync(
    Analysis.ofSourceRealized(`introduction/${ordinal}`, encoder.encode(code)),
  )
  return (
    Analysis.diagnostics(snapshot).length === 0 &&
    Analysis.resolutionFailures(snapshot).length === 0
  )
}

const inline = (nodes: ReadonlyArray<PhrasingContent>): string =>
  nodes
    .map((node) => {
      switch (node.type) {
        case 'text':
          return escapeHtml(node.value)
        case 'inlineCode':
          return `<code>${escapeHtml(node.value)}</code>`
        case 'strong':
          return `<strong>${inline(node.children)}</strong>`
        case 'emphasis':
          return `<em>${inline(node.children)}</em>`
        case 'link':
          return `<a href="${escapeHtml(node.url)}" rel="noreferrer">${inline(node.children)}</a>`
        case 'break':
          return '<br>'
        default:
          return 'children' in node ? inline(node.children as ReadonlyArray<PhrasingContent>) : ''
      }
    })
    .join('')

const plain = (nodes: ReadonlyArray<unknown>): string =>
  nodes
    .map((node) => {
      if (typeof node !== 'object' || node === null) return ''
      if ('value' in node && typeof node.value === 'string') return node.value
      if ('children' in node && Array.isArray(node.children)) return plain(node.children)
      return ''
    })
    .join('')

interface State {
  silkOrdinal: number
  liveNoteDone: boolean
}

const chatCard = (children: ReadonlyArray<BlockContent>): string => {
  const rows: Array<string> = []
  for (const paragraph of children) {
    if (paragraph.type !== 'paragraph') continue
    const text = plain(paragraph.children)
    if (text.startsWith('—')) {
      rows.push(`<footer>${inline(paragraph.children)}</footer>`)
      continue
    }
    const lines: Array<Array<PhrasingContent>> = [[]]
    for (const child of paragraph.children) {
      if (child.type === 'break') lines.push([])
      else lines[lines.length - 1]?.push(child)
    }
    let current: string | undefined
    for (const line of lines) {
      const first = line[0]
      if (line.length === 1 && first?.type === 'strong') {
        if (current !== undefined) rows.push(`${current}</div></div>`)
        current = `<div class="msg"><div class="who">${inline(first.children)}</div><div class="said">`
      } else if (line.length > 0) {
        if (current === undefined) current = '<div class="msg"><div class="said">'
        current += `<p>${inline(line)}</p>`
      }
    }
    if (current !== undefined) rows.push(`${current}</div></div>`)
  }
  return `<figure class="chat">${rows.join('')}</figure>`
}

const block = (node: RootContent | BlockContent, state: State): string => {
  switch (node.type) {
    case 'heading':
      return `<h${node.depth}>${inline(node.children)}</h${node.depth}>`
    case 'paragraph':
      return `<p>${inline(node.children)}</p>`
    case 'blockquote': {
      const text = plain(node.children)
      if (text.includes('Michael Arnaldi')) return chatCard(node.children as never)
      if (text.startsWith('Status:'))
        return `<aside class="status"><span class="stage">stage 0</span>${node.children
          .map((child) => block(child, state))
          .join('')}</aside>`
      const only = node.children.length === 1 ? node.children[0] : undefined
      if (
        only?.type === 'paragraph' &&
        only.children.length === 1 &&
        only.children[0]?.type === 'strong'
      )
        return `<blockquote class="thread"><p>${inline(only.children[0].children)}</p></blockquote>`
      return `<blockquote>${node.children.map((child) => block(child, state)).join('')}</blockquote>`
    }
    case 'code': {
      if (node.lang === 'silk') {
        state.silkOrdinal += 1
        const live = compiles(node.value, state.silkOrdinal)
        const badge = live
          ? '<span class="live">live · hover for types</span>'
          : '<span class="frag">fragment</span>'
        let out = `<figure class="pane"><div class="panebar"><span class="lang">silk</span><span class="spacer"></span>${badge}</div><silk-snippet${live ? ' diagnostics hover' : ''}>\n${escapeHtml(node.value)}</silk-snippet></figure>`
        if (live && !state.liveNoteDone) {
          state.liveNoteDone = true
          out +=
            '<p class="live-note">Live means the Silk compiler is running in your browser: hover a name for its type and docs — underlines are real diagnostics. Fragments are illustrative sketches and are not compiled.</p>'
        }
        return out
      }
      return `<pre class="diagram"><code>${escapeHtml(node.value)}</code></pre>`
    }
    case 'thematicBreak':
      return '<hr>'
    default:
      return 'children' in node
        ? (node.children as ReadonlyArray<BlockContent>).map((child) => block(child, state)).join('\n')
        : ''
  }
}

/** Renders the whole essay to the HTML the landing page injects. */
export const render = (markdown: string): string => {
  const state: State = { silkOrdinal: 0, liveNoteDone: false }
  return fromMarkdown(markdown)
    .children.map((child) => block(child, state))
    .join('\n')
}
