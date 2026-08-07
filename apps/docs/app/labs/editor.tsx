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
import { setDiagnostics } from '@codemirror/lint'
import { Annotation, EditorState, StateEffect, StateField } from '@codemirror/state'
import type { DecorationSet } from '@codemirror/view'
import { Decoration, EditorView, hoverTooltip, keymap } from '@codemirror/view'
import type * as Analysis from '@silk-effect/compiler/Analysis'
import * as Type from '@silk-effect/compiler/Type'
import * as SilkCodeMirror from '@silk-effect/language/CodeMirror'
import * as LspDocument from '@silk-effect/lsp/Document'
import * as Effect from 'effect/Effect'
import { type MutableRefObject, useEffect, useMemo, useRef } from 'react'
import type { Span } from './row/row'

const encoder = new TextEncoder()

/** One language-server view of the active module, rebuilt with the shared snapshot. */
interface LspSession {
  readonly document: LspDocument.Document
  readonly snapshot: Analysis.Snapshot
}

/** Translates one protocol position into a clamped CodeMirror character offset. */
const lspOffset = (
  state: EditorState,
  position: { readonly line: number; readonly character: number },
): number => {
  const line = state.doc.line(Math.min(position.line + 1, state.doc.lines))
  return Math.min(line.from + position.character, line.to)
}

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
  '.cm-lintRange-error': {
    backgroundImage: 'none',
    textDecoration: 'underline wavy var(--wb-error) 1px',
    textUnderlineOffset: '3px',
  },
  '.cm-tooltip': {
    backgroundColor: 'var(--wb-bg-pane)',
    border: '1px solid var(--wb-hairline-strong)',
    color: 'var(--wb-ink-2)',
    fontFamily: 'var(--wb-font)',
    fontSize: '11px',
  },
  '.cm-tooltip.cm-tooltip-hover': { padding: '3px 7px' },
  '.cm-tooltip-lint': { padding: '0' },
  '.cm-diagnostic': { borderLeft: 'none', padding: '3px 7px' },
  '.cm-diagnostic-error': { borderLeft: '2px solid var(--wb-error)' },
})

export function SilkEditor(props: {
  readonly value: string
  readonly snapshot: Analysis.Snapshot
  readonly module: string
  readonly cursor?: Span | undefined
  readonly onChange: (value: string) => void
  readonly onSelect: (span: Span) => void
  /** Receives the format command, so a control outside the editor can trigger it. */
  readonly formatRef?: MutableRefObject<(() => boolean) | null>
  readonly className?: string
}) {
  const containerRef = useRef<HTMLDivElement | null>(null)
  const viewRef = useRef<EditorView | null>(null)
  const initialRef = useRef(props.value)
  const callbacksRef = useRef({ onChange: props.onChange, onSelect: props.onSelect })
  callbacksRef.current = { onChange: props.onChange, onSelect: props.onSelect }

  // The language-server session mirrors the snapshot the whole workbench shares.
  const session = useMemo<LspSession>(
    () => ({
      document: LspDocument.make({
        uri: props.module,
        module: props.module,
        sourceRoot: '/',
        bytes: encoder.encode(props.value),
      }),
      snapshot: props.snapshot,
    }),
    [props.module, props.value, props.snapshot],
  )
  const sessionRef = useRef(session)
  sessionRef.current = session

  // Canonical formatting through the language server's Document actor. Reads only refs, so the
  // one instance created on mount stays valid for the editor's whole life.
  const formatRef = useRef<() => boolean>(() => false)
  formatRef.current = () => {
    const view = viewRef.current
    if (view === null) return false
    const { document: lspDocument, snapshot } = sessionRef.current
    const edit = Effect.runSync(LspDocument.format(lspDocument, snapshot))[0]
    if (edit === undefined) return false
    view.dispatch({
      changes: {
        from: lspOffset(view.state, edit.range.start),
        to: lspOffset(view.state, edit.range.end),
        insert: edit.newText,
      },
    })
    return true
  }
  if (props.formatRef !== undefined) props.formatRef.current = () => formatRef.current()

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
    // Hover asks the language server's Document actor for the smallest typed fact.
    const typeHover = hoverTooltip((view, position) => {
      const { document: lspDocument, snapshot } = sessionRef.current
      const line = view.state.doc.lineAt(position)
      const typed = LspDocument.typeAt(lspDocument, snapshot, {
        line: line.number - 1,
        character: position - line.from,
      })
      if (typed === undefined) return null
      const doc = view.state.doc.toString()
      return {
        pos: SilkCodeMirror.byteOffsetToCharOffset(doc, typed.span.start),
        end: SilkCodeMirror.byteOffsetToCharOffset(doc, typed.span.end),
        above: true,
        create: () => {
          const dom = document.createElement('div')
          dom.className = 'cm-silk-type-tooltip'
          dom.textContent = Type.encode(typed.type)
          return { dom }
        },
      }
    })
    const view = new EditorView({
      parent: container,
      state: EditorState.create({
        doc: initialRef.current,
        extensions: [
          history(),
          keymap.of([
            { key: 'Shift-Alt-f', run: () => formatRef.current() },
            // Format on save: there is nothing to save, but the muscle memory is universal.
            // Always handled, so the browser's save dialog never opens over the workbench.
            {
              key: 'Mod-s',
              run: () => {
                formatRef.current()
                return true
              },
            },
            ...defaultKeymap,
            ...historyKeymap,
          ]),
          SilkCodeMirror.extension(),
          spanCursorField,
          typeHover,
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

  // Diagnostics ride the shared snapshot: every edit re-analyzes, every analysis re-lints.
  // Runs after the sync effect above, so the view's doc always matches the linted value.
  useEffect(() => {
    const view = viewRef.current
    if (view === null || view.state.doc.toString() !== props.value) return
    const diagnostics = LspDocument.diagnostics(session.document, session.snapshot, () => undefined)
    view.dispatch(
      setDiagnostics(
        view.state,
        diagnostics.map((diagnostic) => ({
          from: lspOffset(view.state, diagnostic.range.start),
          to: lspOffset(view.state, diagnostic.range.end),
          severity: 'error' as const,
          message:
            typeof diagnostic.code === 'string'
              ? `${diagnostic.code}: ${diagnostic.message}`
              : diagnostic.message,
        })),
      ),
    )
  }, [session, props.value])

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
