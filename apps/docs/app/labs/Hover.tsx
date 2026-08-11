import * as SilkCodeMirror from '@silk-effect/language/CodeMirror'
import type { BlockContent, PhrasingContent, RootContent } from 'mdast'
import { fromMarkdown } from 'mdast-util-from-markdown'
import { createElement, Fragment, type ReactNode } from 'react'

const safeLink = (destination: string): string | undefined => {
  const normalized = destination.trim()
  if (normalized.startsWith('//')) return undefined
  const scheme = /^([a-z][a-z\d+.-]*):/i.exec(normalized)?.[1]?.toLowerCase()
  return scheme === undefined || scheme === 'http' || scheme === 'https' || scheme === 'mailto'
    ? destination
    : undefined
}

const inlines = (children: ReadonlyArray<PhrasingContent>): ReadonlyArray<ReactNode> =>
  children.map((child, index) => inline(child, index))

const inline = (node: PhrasingContent, key: number): ReactNode => {
  switch (node.type) {
    case 'text':
      return <Fragment key={key}>{node.value}</Fragment>
    case 'inlineCode':
      return <code key={key}>{node.value}</code>
    case 'emphasis':
      return <em key={key}>{inlines(node.children)}</em>
    case 'strong':
      return <strong key={key}>{inlines(node.children)}</strong>
    case 'link': {
      const href = safeLink(node.url)
      return href === undefined ? (
        <Fragment key={key}>{inlines(node.children)}</Fragment>
      ) : (
        <a key={key} href={href} title={node.title ?? undefined} rel="noreferrer">
          {inlines(node.children)}
        </a>
      )
    }
    case 'linkReference':
      return <Fragment key={key}>{inlines(node.children)}</Fragment>
    case 'break':
      return <br key={key} />
    case 'image': {
      const href = safeLink(node.url)
      return href === undefined ? (
        <Fragment key={key}>{node.alt}</Fragment>
      ) : (
        <a key={key} href={href} title={node.title ?? undefined} rel="noreferrer">
          {node.alt}
        </a>
      )
    }
    case 'imageReference':
      return <Fragment key={key}>{node.alt}</Fragment>
    case 'html':
      return <Fragment key={key}>{node.value}</Fragment>
    default:
      return null
  }
}

const silk = (value: string): ReadonlyArray<ReactNode> => {
  const result: Array<ReactNode> = []
  let offset = 0
  for (const range of SilkCodeMirror.highlightRanges(value)) {
    if (range.from < offset || range.to <= range.from) continue
    if (range.from > offset) result.push(value.slice(offset, range.from))
    result.push(
      <span key={`${range.from}:${range.to}`} className={`cm-silk-${range.category}`}>
        {value.slice(range.from, range.to)}
      </span>,
    )
    offset = range.to
  }
  if (offset < value.length) result.push(value.slice(offset))
  return result
}

const block = (node: RootContent | BlockContent, key: number): ReactNode => {
  switch (node.type) {
    case 'paragraph':
      return <p key={key}>{inlines(node.children)}</p>
    case 'heading':
      return createElement(`h${node.depth}`, { key }, inlines(node.children))
    case 'code':
      return (
        <pre key={key} data-language={node.lang ?? undefined}>
          <code>{node.lang?.toLowerCase() === 'silk' ? silk(node.value) : node.value}</code>
        </pre>
      )
    case 'blockquote':
      return (
        <blockquote key={key}>
          {node.children.map((child, index) => block(child, index))}
        </blockquote>
      )
    case 'list': {
      const List = node.ordered ? 'ol' : 'ul'
      return (
        <List key={key} start={node.ordered ? (node.start ?? undefined) : undefined}>
          {node.children.map((item, itemIndex) => (
            <li key={itemIndex}>
              {item.children.map((child, childIndex) => block(child, childIndex))}
            </li>
          ))}
        </List>
      )
    }
    case 'thematicBreak':
      return <hr key={key} />
    case 'html':
      return <p key={key}>{node.value}</p>
    case 'definition':
      return null
    default:
      return null
  }
}

/** Safely renders the CommonMark payload supplied by one language-server hover. */
export function Content(props: { readonly markdown: string }) {
  const root = fromMarkdown(props.markdown)
  return (
    <div className="cm-silk-type-tooltip">
      {root.children.map((child, index) => block(child, index))}
    </div>
  )
}
