'use client'

/**
 * The one row every inspector is built from.
 *
 * Each phase used to render its own card markup, so comparing two phases meant reading two
 * different visual languages and scrolling six times as far. A shared row makes the phases
 * genuinely comparable: the same construct occupies the same shape everywhere, and the only
 * thing that differs between panes is the content of the columns.
 */

import type { ReactNode } from 'react'
import styles from './row.module.css'

/** A byte range in one module's source. The join between every phase. */
export interface Span {
  readonly start: number
  readonly end: number
}

export type RowTone = 'error' | 'warning' | 'symbol' | 'ok'
export type DotKind = 'token' | 'node' | 'missing' | 'error' | 'warning' | 'ok' | 'symbol'

export interface RowModel {
  readonly key: string
  /** Optional line/token index, rendered in the 26px tabular lead column. */
  readonly lead?: string | number
  readonly depth?: number
  /** `undefined` renders no caret; a branch row supplies `▾`/`▸`. */
  readonly caret?: string
  readonly dot?: DotKind
  readonly label: string
  readonly detail?: string
  readonly span?: Span
  /** Group heads render at 700 weight — the row grammar's only typographic emphasis. */
  readonly head?: boolean
  readonly tone?: RowTone
  readonly onActivate?: () => void
}

const INDENT_STEP = 13

/**
 * Where the span cursor sits relative to a row.
 *
 * `contains` is deliberately not `equals`: clicking `identity` in the token stream should tint
 * the enclosing `CallExpression` in the tree and the function that lowered it in MIR, because
 * those are the same construct seen through later phases.
 */
export type CursorState = 'exact' | 'contains' | undefined

export const cursorStateFor = (span: Span | undefined, cursor: Span | undefined): CursorState => {
  if (span === undefined || cursor === undefined) return undefined
  if (span.start === cursor.start && span.end === cursor.end) return 'exact'
  return span.start <= cursor.start && span.end >= cursor.end ? 'contains' : undefined
}

export const spanLabel = (span: Span | undefined): string =>
  span === undefined ? '' : `${span.start}–${span.end}`

export function Row({
  model,
  cursor,
}: {
  readonly model: RowModel
  readonly cursor?: Span | undefined
}) {
  const state = cursorStateFor(model.span, cursor)
  const depth = model.depth ?? 0
  const pickable = model.onActivate !== undefined

  // A row that moves the cursor is a button; a row that only displays is not, so the keyboard
  // walks exactly the rows that do something.
  const Element = pickable ? 'button' : 'div'

  return (
    <Element
      className={styles.row}
      {...(pickable ? { type: 'button' as const, onClick: model.onActivate } : {})}
      data-pickable={pickable}
      data-cursor={state}
      data-tone={model.tone}
      data-head={model.head}
      {...(state === 'exact' ? { 'aria-current': 'true' as const } : {})}
    >
      {model.lead === undefined ? null : <span className={styles.lead}>{model.lead}</span>}
      {depth === 0 ? null : (
        <span className={styles.indent} style={{ width: `${depth * INDENT_STEP}px` }} />
      )}
      <span className={styles.caret}>{model.caret ?? ''}</span>
      <i className={styles.dot} data-kind={model.dot} aria-hidden="true" />
      <span className={styles.label} data-tone={model.tone}>
        {model.label}
      </span>
      <span className={styles.detail}>{model.detail ?? ''}</span>
      <span className={styles.span}>{spanLabel(model.span)}</span>
    </Element>
  )
}

/**
 * A scrolling list of rows.
 *
 * Rows carry their own span, so wiring the cursor is this component's job rather than each
 * inspector's — a pane hands over rows and gets cross-pane linking for free.
 */
export function RowList({
  rows,
  cursor,
  label,
}: {
  readonly rows: ReadonlyArray<RowModel>
  readonly cursor?: Span | undefined
  readonly label: string
}) {
  return (
    <div className={styles.rows} role="list" aria-label={label}>
      {rows.map((row) => (
        <Row key={row.key} model={row} cursor={cursor} />
      ))}
    </div>
  )
}

/**
 * Why a pane is empty, rather than an empty pane.
 *
 * A blank MIR pane and a MIR pane for a program that never elaborated look identical, which
 * hides the phase that actually failed. Naming the reason is what makes a broken pipeline
 * readable from the panes alone.
 */
export function EmptyState({
  children,
  reason,
}: {
  readonly children: ReactNode
  readonly reason?: boolean
}) {
  return (
    <p className={`${styles.empty} ${reason === true ? styles.emptyReason : ''}`.trim()}>
      {children}
    </p>
  )
}
