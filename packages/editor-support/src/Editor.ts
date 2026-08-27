/**
 * The framework-free snippet editor: CodeMirror with lexer-driven Silk highlighting and opt-in
 * language-server semantics, extracted from the docs labs workbench editor.
 *
 * The compiler's spans are byte-addressed while CodeMirror's are UTF-16, so every range is
 * translated at this boundary. Semantic features read one immutable session — the language-server
 * document paired with the exact analysis snapshot of its bytes — and go quiet whenever the
 * visible document has moved ahead of analysis, exactly as the labs editor behaved.
 */

import { defaultKeymap, history, historyKeymap } from '@codemirror/commands'
import { setDiagnostics } from '@codemirror/lint'
import type { Extension } from '@codemirror/state'
import { Annotation, EditorState, StateEffect, StateField } from '@codemirror/state'
import type { DecorationSet } from '@codemirror/view'
import { Decoration, EditorView, hoverTooltip, keymap, WidgetType } from '@codemirror/view'
import type * as Analysis from '@silklang/compiler/Analysis'
import * as LspDocument from '@silklang/lsp/Document'
import * as LineIndex from '@silklang/lsp/LineIndex'
import * as Effect from 'effect/Effect'
import type { InlayHint } from 'vscode-languageserver-types'
import * as SilkCodeMirror from './CodeMirror.js'
import * as HoverContent from './HoverContent.js'

/** The semantic features one mounted editor exposes. Highlighting is always on. */
export interface Features {
  readonly diagnostics?: boolean
  readonly hover?: boolean
  readonly inlayHints?: boolean
}

export interface Options {
  readonly parent: Element | DocumentFragment
  readonly doc: string
  /** Snippets are read-only unless explicitly made editable. */
  readonly editable?: boolean
  readonly features?: Features
  readonly onChange?: (doc: string) => void
  /** Byte ranges only — the caller attaches the module identity it knows and the editor does not. */
  readonly onSelect?: (range: { readonly start: number; readonly end: number }) => void
  /** Pass the shadow root when mounting inside one, so CodeMirror wires events correctly. */
  readonly root?: Document | ShadowRoot
}

/** One language-server view of the snippet: document and snapshot built from the same bytes. */
export interface Session {
  readonly document: LspDocument.Document
  readonly snapshot: Analysis.Snapshot
  readonly source: string
}

/** The imperative surface a host (custom element or React wrapper) drives the editor through. */
export interface Handle {
  value(): string
  setValue(doc: string): void
  /** Installs the session every enabled semantic feature reads; `undefined` clears semantics. */
  setSession(session: Session | undefined): void
  /** Draws the shared span highlight over one byte range; `null` clears it. */
  setSpanHighlight(range: { readonly start: number; readonly end: number } | null): void
  /** Formats through the language server; false when the session is stale or nothing changed. */
  format(): boolean
  destroy(): void
}

/** Builds the session for one snippet from its already-analyzed bytes. */
export const session = (
  module: string,
  bytes: Uint8Array,
  snapshot: Analysis.Snapshot,
): Session => ({
  document: LspDocument.make({
    uri: module,
    version: 0,
    workspace: `snippet:${module}`,
    module,
    sourceRoot: '/',
    bytes,
  }),
  snapshot,
  source: new TextDecoder().decode(bytes),
})

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

const hintText = (hint: InlayHint): string =>
  typeof hint.label === 'string' ? hint.label : hint.label.map((part) => part.value).join('')

/** An inlay hint is presentation only: a widget beside the text, never part of the document. */
class HintWidget extends WidgetType {
  constructor(readonly text: string) {
    super()
  }
  override eq(other: HintWidget): boolean {
    return other.text === this.text
  }
  override toDOM(): HTMLElement {
    const span = document.createElement('span')
    span.className = 'cm-silk-inlay-hint'
    span.textContent = this.text
    return span
  }
  override ignoreEvent(): boolean {
    return true
  }
}

const setInlayHints = StateEffect.define<DecorationSet>()

const inlayHintField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update: (value, transaction) => {
    let next = value.map(transaction.changes)
    for (const effect of transaction.effects) if (effect.is(setInlayHints)) next = effect.value
    return next
  },
  provide: (self) => EditorView.decorations.from(self),
})

/**
 * Editor chrome expressed through `--silk-snippet-*` custom properties, with light defaults, so a
 * host page themes every snippet at once and the element supplies dark values from the reader's
 * color scheme.
 */
const theme = EditorView.theme({
  '&': {
    fontSize: 'var(--silk-snippet-font-size, 13px)',
    backgroundColor: 'transparent',
    color: 'var(--silk-snippet-ink, #1f2328)',
  },
  '.cm-scroller': {
    fontFamily:
      'var(--silk-snippet-font, ui-monospace, SFMono-Regular, Menlo, Consolas, monospace)',
    lineHeight: 'var(--silk-snippet-line-height, 1.55)',
  },
  '.cm-content': {
    padding: 'var(--silk-snippet-padding, 10px 0 10px 12px)',
    caretColor: 'var(--silk-snippet-ink, #1f2328)',
  },
  '&.cm-focused': { outline: 'none' },
  '.cm-cursor': { borderLeftColor: 'var(--silk-snippet-ink, #1f2328)' },
  '.cm-selectionBackground, &.cm-focused .cm-selectionBackground': {
    backgroundColor: 'var(--silk-snippet-selection, rgba(84, 120, 198, 0.2))',
  },
  '.cm-silk-span-cursor': {
    backgroundColor: 'var(--silk-snippet-selection, rgba(84, 120, 198, 0.2))',
  },
  '.cm-silk-invalid': {
    textDecoration: 'underline wavy var(--silk-snippet-error, #d1383d)',
  },
  '.cm-silk-doc-comment': { fontStyle: 'italic' },
  '.cm-silk-inlay-hint': {
    color: 'var(--silk-snippet-hint, #7a8290)',
    opacity: '0.85',
    fontSize: '0.9em',
    pointerEvents: 'none',
  },
  '.cm-lintRange-error': {
    backgroundImage: 'none',
    textDecoration: 'underline wavy var(--silk-snippet-error, #d1383d) 1px',
    textUnderlineOffset: '3px',
  },
  '.cm-tooltip': {
    backgroundColor: 'var(--silk-snippet-tooltip-bg, #ffffff)',
    border: '1px solid var(--silk-snippet-border, #d0d7de)',
    color: 'var(--silk-snippet-ink-muted, #424a53)',
    fontFamily:
      'var(--silk-snippet-font, ui-monospace, SFMono-Regular, Menlo, Consolas, monospace)',
    fontSize: '0.92em',
    // White-space is inherited and crosses the shadow boundary, so a host page that sets
    // `white-space: pre` on the element (a common no-JS fallback style) must not stop tooltip
    // prose from wrapping.
    whiteSpace: 'normal',
  },
  '.cm-tooltip.cm-tooltip-hover': { padding: '3px 7px' },
  '.cm-tooltip-lint': { padding: '0' },
  '.cm-diagnostic': { borderLeft: 'none', padding: '3px 7px' },
  '.cm-diagnostic-error': {
    borderLeft: '2px solid var(--silk-snippet-error, #d1383d)',
  },
  '.cm-silk-type-tooltip': {
    boxSizing: 'border-box',
    maxWidth: 'min(560px, calc(100vw - 24px))',
    // Long documentation scrolls inside the tooltip instead of growing past the viewport.
    maxHeight: 'min(340px, 45vh)',
    overflowY: 'auto',
    overscrollBehavior: 'contain',
    lineHeight: '1.45',
  },
  '.cm-silk-type-tooltip > *': { margin: '0' },
  '.cm-silk-type-tooltip > * + *': { marginTop: '8px' },
  '.cm-silk-type-tooltip pre': {
    overflowX: 'auto',
    padding: '2px 0 5px',
    borderBottom: '1px solid var(--silk-snippet-border, #d0d7de)',
    color: 'var(--silk-snippet-ink, #1f2328)',
    whiteSpace: 'pre',
  },
  '.cm-silk-type-tooltip :not(pre) > code': {
    padding: '1px 3px',
    borderRadius: '2px',
    backgroundColor: 'var(--silk-snippet-code-bg, rgba(84, 120, 198, 0.12))',
    color: 'var(--silk-snippet-ink, #1f2328)',
  },
  '.cm-silk-type-tooltip h1, .cm-silk-type-tooltip h2, .cm-silk-type-tooltip h3, .cm-silk-type-tooltip h4, .cm-silk-type-tooltip h5, .cm-silk-type-tooltip h6':
    {
      color: 'var(--silk-snippet-ink, #1f2328)',
      fontSize: 'inherit',
      fontWeight: '600',
    },
  '.cm-silk-type-tooltip ul, .cm-silk-type-tooltip ol': {
    marginBottom: '0',
    paddingLeft: '20px',
  },
  '.cm-silk-type-tooltip li + li': { marginTop: '3px' },
  '.cm-silk-type-tooltip blockquote': {
    paddingLeft: '8px',
    borderLeft: '2px solid var(--silk-snippet-border, #d0d7de)',
    color: 'var(--silk-snippet-ink-muted, #424a53)',
  },
  '.cm-silk-type-tooltip a': {
    color: 'var(--silk-snippet-accent, #5478c6)',
    textDecoration: 'underline',
  },
  '.cm-silk-type-tooltip hr': {
    border: '0',
    borderTop: '1px solid var(--silk-snippet-border, #d0d7de)',
  },
})

/** Mounts one snippet editor and returns the handle its host drives it through. */
export const mount = (options: Options): Handle => {
  const features = options.features ?? {}
  let current: Session | undefined

  const applyDiagnostics = (view: EditorView): void => {
    if (features.diagnostics !== true) return
    const session_ = current
    if (session_ === undefined || view.state.doc.toString() !== session_.source) {
      view.dispatch(setDiagnostics(view.state, []))
      return
    }
    const diagnostics = LspDocument.diagnostics(
      session_.document,
      session_.snapshot,
      () => undefined,
    )
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
  }

  const applyInlayHints = (view: EditorView): void => {
    if (features.inlayHints !== true) return
    const session_ = current
    if (session_ === undefined || view.state.doc.toString() !== session_.source) {
      view.dispatch({ effects: setInlayHints.of(Decoration.none) })
      return
    }
    const hints = LspDocument.inlayHints(
      session_.document,
      session_.snapshot,
      LineIndex.fullRange(session_.document.index),
    )
    const decorations = hints
      .map((hint) => ({ offset: lspOffset(view.state, hint.position), hint }))
      .sort((left, right) => left.offset - right.offset)
      .map(({ offset, hint }) =>
        Decoration.widget({ widget: new HintWidget(hintText(hint)), side: 1 }).range(offset),
      )
    view.dispatch({ effects: setInlayHints.of(Decoration.set(decorations)) })
  }

  const typeHover = hoverTooltip(
    (view, position) => {
      const session_ = current
      if (session_ === undefined || view.state.doc.toString() !== session_.source) return null
      const line = view.state.doc.lineAt(position)
      const hover = LspDocument.hover(session_.document, session_.snapshot, {
        line: line.number - 1,
        character: position - line.from,
      })
      if (
        hover?.range === undefined ||
        typeof hover.contents !== 'object' ||
        !('value' in hover.contents)
      )
        return null
      const hoverText = hover.contents.value
      return {
        pos: lspOffset(view.state, hover.range.start),
        end: lspOffset(view.state, hover.range.end),
        above: true,
        create: () => ({ dom: HoverContent.render(hoverText) }),
      }
    },
    // The query is a synchronous lookup against an already-built snapshot, so any rest delay is
    // the only wait a reader ever feels; zero shows the tooltip the moment the pointer stops.
    { hoverTime: 0 },
  )

  const format = (): boolean => {
    const session_ = current
    if (session_ === undefined || view.state.doc.toString() !== session_.source) return false
    const edit = Effect.runSync(LspDocument.format(session_.document, session_.snapshot))[0]
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

  const listener = EditorView.updateListener.of((update) => {
    const external = update.transactions.some(
      (transaction) => transaction.annotation(External) === true,
    )
    if (external) return
    if (update.docChanged) options.onChange?.(update.state.doc.toString())
    if (update.selectionSet && options.onSelect !== undefined) {
      const range = update.state.selection.main
      if (range.empty) return
      const doc = update.state.doc.toString()
      options.onSelect({
        start: SilkCodeMirror.charOffsetToByteOffset(doc, range.from),
        end: SilkCodeMirror.charOffsetToByteOffset(doc, range.to),
      })
    }
  })

  const editable = options.editable === true
  // The stable-class field only, not `extension()`: its default-highlight-style fallback is
  // light-only, and the host (element shadow styles, or the labs stylesheet) owns token colors so
  // one palette can serve both color schemes.
  const extensions: Array<Extension> = [SilkCodeMirror.field, spanCursorField, theme]
  if (editable) {
    extensions.push(
      history(),
      keymap.of([
        { key: 'Shift-Alt-f', run: format },
        // Format on save: there is nothing to save, but the muscle memory is universal.
        {
          key: 'Mod-s',
          run: () => {
            format()
            return true
          },
        },
        ...defaultKeymap,
        ...historyKeymap,
      ]),
    )
  } else {
    extensions.push(EditorState.readOnly.of(true), EditorView.editable.of(false))
  }
  if (features.hover === true) extensions.push(typeHover)
  if (features.inlayHints === true) extensions.push(inlayHintField)
  extensions.push(EditorView.contentAttributes.of({ 'aria-label': 'Silk source code' }), listener)

  const view = new EditorView({
    parent: options.parent,
    ...(options.root === undefined ? {} : { root: options.root }),
    state: EditorState.create({ doc: options.doc, extensions }),
  })

  return {
    value: () => view.state.doc.toString(),
    setValue: (doc) => {
      const existing = view.state.doc.toString()
      if (existing === doc) return
      view.dispatch({
        changes: { from: 0, to: existing.length, insert: doc },
        annotations: External.of(true),
      })
    },
    setSession: (session_) => {
      current = session_
      applyDiagnostics(view)
      applyInlayHints(view)
    },
    setSpanHighlight: (range) => {
      if (range === null) {
        view.dispatch({ effects: setSpanCursor.of(null) })
        return
      }
      const doc = view.state.doc.toString()
      const from = SilkCodeMirror.byteOffsetToCharOffset(doc, range.start)
      const to = SilkCodeMirror.byteOffsetToCharOffset(doc, range.end)
      if (to <= from) {
        view.dispatch({ effects: setSpanCursor.of(null) })
        return
      }
      const selection = view.state.selection.main
      const effects: Array<StateEffect<unknown>> = [setSpanCursor.of({ from, to })]
      if (selection.from !== from || selection.to !== to)
        effects.push(EditorView.scrollIntoView(from))
      view.dispatch({ effects })
    },
    format,
    destroy: () => view.destroy(),
  }
}
