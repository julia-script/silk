'use client'

/**
 * The React shell around the shared row grammar.
 *
 * The row model itself lives in `@silk-lang/inspector` — rows are pure data with
 * module-qualified spans, so the same projections serve this workbench and the language server.
 * What stays here is rendering and activation: a row is pickable exactly when it carries a span,
 * and picking one moves the shared span cursor.
 */

import type { RowModel, Span } from '@silk-lang/inspector'
import { cursorStateFor, spanLabel } from '@silk-lang/inspector'
import type { ReactNode } from 'react'
import styles from './row.module.css'

const INDENT_STEP = 13

export function Row({
  model,
  cursor,
  onPick,
}: {
  readonly model: RowModel
  readonly cursor?: Span | undefined
  readonly onPick?: ((span: Span) => void) | undefined
}) {
  const state = cursorStateFor(model.span, cursor)
  const depth = model.depth ?? 0
  const span = model.span
  const pickable = span !== undefined && onPick !== undefined

  // A row that moves the cursor is a button; a row that only displays is not, so the keyboard
  // walks exactly the rows that do something.
  const Element = pickable ? 'button' : 'div'

  return (
    <Element
      className={styles.row}
      {...(pickable ? { type: 'button' as const, onClick: () => onPick(span) } : {})}
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
  onPick,
}: {
  readonly rows: ReadonlyArray<RowModel>
  readonly cursor?: Span | undefined
  readonly label: string
  readonly onPick?: ((span: Span) => void) | undefined
}) {
  return (
    <div className={styles.rows} role="list" aria-label={label}>
      {rows.map((row) => (
        <Row key={row.key} model={row} cursor={cursor} onPick={onPick} />
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
