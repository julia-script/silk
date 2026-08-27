/**
 * Renders the introduction essay's Markdown as the landing page's HTML.
 *
 * Every `silk` fence is compiled at render time with the same convention doctest verifies —
 * one standalone module, default target — and only fences that compile cleanly become live
 * snippets with diagnostics and hover. Fragments stay highlight-only, so the page can never
 * show a false diagnostic, and the live set can never go stale against the compiler.
 */

import * as Analysis from '@silk-lang/compiler/Analysis'
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
  // A speaker is a strong-only line; everything after it, across paragraphs and hard breaks,
  // belongs to that speaker's message until the next speaker or the attribution line.
  const rows: Array<string> = []
  let current: string | undefined
  const closeCurrent = (): void => {
    if (current !== undefined) rows.push(`${current}</div></div>`)
    current = undefined
  }
  for (const paragraph of children) {
    if (paragraph.type !== 'paragraph') continue
    const text = plain(paragraph.children)
    if (text.startsWith('—')) {
      closeCurrent()
      rows.push(`<footer>${inline(paragraph.children)}</footer>`)
      continue
    }
    const lines: Array<Array<PhrasingContent>> = [[]]
    for (const child of paragraph.children) {
      if (child.type === 'break') lines.push([])
      else lines[lines.length - 1]?.push(child)
    }
    for (const line of lines) {
      const first = line[0]
      if (line.length === 1 && first?.type === 'strong') {
        closeCurrent()
        current = `<div class="msg"><div class="who">${inline(first.children)}</div><div class="said">`
      } else if (line.length > 0) {
        if (current === undefined) current = '<div class="msg"><div class="said">'
        current += `<p>${inline(line)}</p>`
      }
    }
  }
  closeCurrent()
  return `<figure class="chat">${rows.join('')}</figure>`
}

/**
 * The signature-anatomy graphic: `Effect<A ! E ? R>` with its three channels color-coded and
 * drawn out with real connectors instead of box-drawing characters. JetBrains Mono's advance
 * width is exactly 0.6em and the signature text is pinned with `textLength`, so the connector
 * anchors sit under their letters on every platform. Colors come from the page's own tokens.
 */
const signatureAnatomy = (): string => {
  const size = 24
  const cw = size * 0.6
  const x0 = 40
  const baseline = 48
  // Character offsets inside `Effect<A ! E ? R>`: A at 7, E at 11, R at 15.
  const anchor = (index: number): number => x0 + (index + 0.5) * cw
  const a = anchor(7)
  const e = anchor(11)
  const r = anchor(15)
  const rows: ReadonlyArray<{
    readonly x: number
    readonly y: number
    readonly color: string
    readonly chip: string
    readonly text: string
  }> = [
    { x: r, y: 104, color: 'var(--amber)', chip: '? R', text: 'what the computation requires' },
    { x: e, y: 148, color: 'var(--error-ink)', chip: '! E', text: 'how the computation may fail' },
    { x: a, y: 192, color: 'var(--green-ink)', chip: 'A', text: 'what the computation may produce' },
  ]
  const connectors = rows
    .map(
      (row) =>
        `<circle cx="${row.x}" cy="${baseline + 12}" r="2.5" fill="${row.color}"/>` +
        `<path d="M ${row.x} ${baseline + 12} L ${row.x} ${row.y - 12} Q ${row.x} ${row.y - 4} ${row.x + 8} ${row.y - 4} L 300 ${row.y - 4}" fill="none" stroke="${row.color}" stroke-opacity="0.55" stroke-width="1.5"/>`,
    )
    .join('')
  const labels = rows
    .map(
      (row) =>
        `<text x="312" y="${row.y}" font-size="13" fill="${row.color}" font-weight="500">${row.chip}</text>` +
        `<text x="${row.chip === 'A' ? 328 : 348}" y="${row.y}" font-size="13" fill="var(--ink-2)">${row.text}</text>`,
    )
    .join('')
  return `<figure class="anatomy" aria-label="Anatomy of Effect of A, failing with E, requiring R"><svg viewBox="0 0 640 216" role="img">
<text x="${x0}" y="${baseline}" font-size="${size}" textLength="${17 * cw}" lengthAdjust="spacing" fill="var(--ink-3)"><tspan>Effect&lt;</tspan><tspan fill="var(--green-ink)">A</tspan><tspan> </tspan><tspan fill="var(--error-ink)">! E</tspan><tspan> </tspan><tspan fill="var(--amber)">? R</tspan><tspan>&gt;</tspan></text>
${connectors}
${labels}
</svg></figure>`
}

/** The service contract diagram: a requirement met by an implementation, drawn as a flow. */
const contractDiagram = (): string => `<figure class="anatomy" aria-label="readClock requires the Clock contract; FixedClock provides it"><svg viewBox="0 0 640 216" role="img">
<text x="40" y="30" font-size="15" fill="var(--ink)">readClock()</text>
<path d="M 79 42 L 79 78" stroke="var(--amber)" stroke-opacity="0.7" stroke-width="1.5"/>
<polygon points="74,78 84,78 79,88" fill="var(--amber)"/>
<text x="94" y="68" font-size="12" fill="var(--amber)">requires</text>
<text x="40" y="112" font-size="15" fill="var(--amber)">&amp;Clock</text>
<text x="150" y="112" font-size="12" fill="var(--gutter)">· the contract</text>
<path d="M 79 172 L 79 136" stroke="var(--violet)" stroke-opacity="0.7" stroke-width="1.5"/>
<polygon points="74,136 84,136 79,126" fill="var(--violet)"/>
<text x="94" y="158" font-size="12" fill="var(--violet)">provides</text>
<text x="40" y="196" font-size="15" fill="var(--violet)">FixedClock { value: 42 }</text>
<text x="290" y="196" font-size="12" fill="var(--gutter)">· one implementation</text>
</svg></figure>`

/** The structured-concurrency lifetime tree, with elbow connectors instead of box characters. */
const fiberTreeDiagram = (): string => `<figure class="anatomy" aria-label="A root task owning Fiber A and Fiber B, with Fiber C under Fiber A"><svg viewBox="0 0 640 208" role="img">
<text x="320" y="28" font-size="15" fill="var(--ink)" text-anchor="middle">root</text>
<path d="M 320 40 L 320 56 Q 320 64 312 64 L 228 64 Q 220 64 220 72 L 220 88" fill="none" stroke="var(--ink-4)" stroke-width="1.5"/>
<path d="M 320 40 L 320 56 Q 320 64 328 64 L 412 64 Q 420 64 420 72 L 420 88" fill="none" stroke="var(--ink-4)" stroke-width="1.5"/>
<circle cx="320" cy="40" r="2.5" fill="var(--ink-4)"/>
<text x="220" y="112" font-size="15" fill="var(--violet)" text-anchor="middle">Fiber A</text>
<text x="420" y="112" font-size="15" fill="var(--violet)" text-anchor="middle">Fiber B</text>
<path d="M 220 124 L 220 164" stroke="var(--ink-4)" stroke-width="1.5"/>
<text x="220" y="188" font-size="15" fill="var(--violet)" text-anchor="middle">Fiber C</text>
</svg></figure>`

/** Root termination cancelling descendants, as a single emphatic arrow. */
const rootTerminatesDiagram = (): string => `<figure class="anatomy" aria-label="When the root terminates, unfinished descendants cancel"><svg viewBox="0 0 640 128" role="img">
<text x="40" y="30" font-size="15" fill="var(--ink)">root terminates</text>
<path d="M 79 42 L 79 84" stroke="var(--error-ink)" stroke-opacity="0.8" stroke-width="1.5"/>
<polygon points="74,84 84,84 79,94" fill="var(--error-ink)"/>
<text x="40" y="118" font-size="15" fill="var(--error-ink)">unfinished descendants cancel</text>
</svg></figure>`

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
      const langParts = node.lang?.split(',').map((part) => part.trim()) ?? []
      if (langParts.at(0) === 'silk') {
        state.silkOrdinal += 1
        // Authored fences may keep a cosmetic blank line before the closing fence; the snippet
        // shows and compiles the code without it.
        const code = node.value.replace(/\s+$/, '')
        // `silk,live` forces semantics on: it marks an example whose diagnostics are the point —
        // deliberately invalid code whose real compiler error the reader should see.
        const live = langParts.includes('live') || compiles(code, state.silkOrdinal)
        const badge = live
          ? '<span class="live">live · hover for types</span>'
          : '<span class="frag">fragment</span>'
        let out = `<figure class="pane"><div class="panebar"><span class="lang">silk</span><span class="spacer"></span>${badge}</div><silk-snippet${live ? ' diagnostics hover inlay-hints' : ''}>\n${escapeHtml(code)}</silk-snippet></figure>`
        if (live && !state.liveNoteDone) {
          state.liveNoteDone = true
          out +=
            '<p class="live-note">Live means the Silk compiler is running in your browser: hover a name for its type and docs — underlines are real diagnostics. Fragments are illustrative sketches and are not compiled.</p>'
        }
        return out
      }
      if (node.value.startsWith('Effect<A ! E ? R>')) return signatureAnatomy()
      if (node.value.startsWith('readClock()')) return contractDiagram()
      if (node.value.startsWith('root terminates')) return rootTerminatesDiagram()
      if (node.value.trimStart().startsWith('root\n')) return fiberTreeDiagram()
      return `<pre class="diagram"><code>${escapeHtml(node.value.replace(/\s+$/, ''))}</code></pre>`
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
