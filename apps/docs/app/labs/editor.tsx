'use client'

/**
 * The workbench source editor: CodeMirror with lexer-driven Silk highlighting.
 *
 * The compiler's spans are byte-addressed while CodeMirror's are UTF-16, so selections are
 * translated before they reach the shared span cursor. Token colors come from the stable
 * `cm-silk-*` classes styled in workbench.module.css — the workbench is always dark, so it does
 * not use the extension's light default highlight style.
 */

import { defaultKeymap, history, historyKeymap } from '@codemirror/commands'
import { Annotation, EditorState, StateEffect, StateField } from '@codemirror/state'
import type { DecorationSet } from '@codemirror/view'
import { Decoration, EditorView, keymap } from '@codemirror/view'
import * as SilkCodeMirror from '@silk-effect/language/CodeMirror'
import { useEffect, useRef } from 'react'
import type { Span } from './row/row'

/** Marks transactions that reconcile external state, so the update listener does not echo them. */
const External = Annotation.define<boolean>()

/**
 * The shared span cursor drawn into the editor, so a row click in any pane lights up the same
 * bytes here — the editor is a phase like any other, in both directions.
 */
const setSpanCursor = StateEffect.define<{ readonly from: number; readonly to: number } | null>()

const spanCursorMark = Decoration.mark({ class: 'cm-silk-span-cursor' })

const spanCursorField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update: (value, transaction) => {
    let next = value.map(transaction.changes)
    for (const effect of transaction.effects) {
      if (effect.is(setSpanCursor)) {
        next =
          effect.value === null
            ? Decoration.none
            : Decoration.set([spanCursorMark.range(effect.value.from, effect.value.to)])
      }
    }
    return next
  },
  provide: (self) => EditorView.decorations.from(self),
})

const theme = EditorView.theme({
  '&': { height: '100%', fontSize: '11.5px', backgroundColor: 'transparent' },
  '.cm-scroller': { fontFamily: 'var(--wb-font)', lineHeight: '19px' },
  '.cm-content': { padding: '5px 0 5px 8px', caretColor: 'var(--wb-ink)' },
  '&.cm-focused': { outline: 'none' },
  '.cm-cursor': { borderLeftColor: 'var(--wb-ink)' },
  '.cm-selectionBackground, &.cm-focused .cm-selectionBackground': {
    backgroundColor: 'rgba(198, 166, 120, 0.22)',
  },
})

export function SilkEditor(props: {
  readonly value: string
  readonly cursor?: Span | undefined
  readonly onChange: (value: string) => void
  readonly onSelect: (span: Span) => void
  readonly className?: string
}) {
  const containerRef = useRef<HTMLDivElement | null>(null)
  const viewRef = useRef<EditorView | null>(null)
  const initialRef = useRef(props.value)
  const callbacksRef = useRef({ onChange: props.onChange, onSelect: props.onSelect })
  callbacksRef.current = { onChange: props.onChange, onSelect: props.onSelect }

  useEffect(() => {
    const container = containerRef.current
    if (container === null) return
    const listener = EditorView.updateListener.of((update) => {
      const external = update.transactions.some(
        (transaction) => transaction.annotation(External) === true,
      )
      if (external) return
      if (update.docChanged) callbacksRef.current.onChange(update.state.doc.toString())
      if (update.selectionSet) {
        const range = update.state.selection.main
        if (range.empty) return
        // Selecting text moves the same span cursor a row click moves, translated to bytes.
        const doc = update.state.doc.toString()
        callbacksRef.current.onSelect({
          start: SilkCodeMirror.charOffsetToByteOffset(doc, range.from),
          end: SilkCodeMirror.charOffsetToByteOffset(doc, range.to),
        })
      }
    })
    const view = new EditorView({
      parent: container,
      state: EditorState.create({
        doc: initialRef.current,
        extensions: [
          history(),
          keymap.of([...defaultKeymap, ...historyKeymap]),
          SilkCodeMirror.extension(),
          spanCursorField,
          theme,
          EditorView.contentAttributes.of({ 'aria-label': 'Silk source code' }),
          listener,
        ],
      }),
    })
    viewRef.current = view
    return () => {
      viewRef.current = null
      view.destroy()
    }
  }, [])

  // Module switches and preset/URL loads replace the document wholesale.
  useEffect(() => {
    const view = viewRef.current
    if (view === null) return
    const current = view.state.doc.toString()
    if (current !== props.value) {
      view.dispatch({
        changes: { from: 0, to: current.length, insert: props.value },
        annotations: External.of(true),
      })
    }
  }, [props.value])

  // Reflect the shared span cursor, scrolling to it only when it came from another pane.
  const cursor = props.cursor
  useEffect(() => {
    const view = viewRef.current
    if (view === null) return
    if (cursor === undefined) {
      view.dispatch({ effects: setSpanCursor.of(null) })
      return
    }
    const doc = view.state.doc.toString()
    const from = SilkCodeMirror.byteOffsetToCharOffset(doc, cursor.start)
    const to = SilkCodeMirror.byteOffsetToCharOffset(doc, cursor.end)
    if (to <= from) {
      view.dispatch({ effects: setSpanCursor.of(null) })
      return
    }
    const selection = view.state.selection.main
    const effects: Array<StateEffect<unknown>> = [setSpanCursor.of({ from, to })]
    if (selection.from !== from || selection.to !== to) {
      effects.push(EditorView.scrollIntoView(from))
    }
    view.dispatch({ effects })
  }, [cursor])

  return <div ref={containerRef} className={props.className} />
}
