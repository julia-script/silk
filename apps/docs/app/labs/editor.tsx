'use client'

/**
 * The workbench source editor: a thin React wrapper over the shared snippet editor core.
 *
 * The core owns CodeMirror, highlighting, diagnostics, and hover; this wrapper owns everything
 * workbench-shaped — the shared span cursor, the format command handle, and the snapshot the whole
 * workbench shares. Colors ride the `--silk-snippet-*` custom properties mapped to workbench
 * variables in workbench.module.css.
 */

import * as Analysis from '@silklang/compiler/Analysis'
import * as SnippetEditor from '@silklang/editor-support/Editor'
import { type MutableRefObject, useEffect, useRef } from 'react'
import type { Span } from '@silklang/compiler/InspectorRow'

const encoder = new TextEncoder()

export function SilkEditor(props: {
  readonly value: string
  readonly snapshot: Analysis.Snapshot
  readonly module: string
  readonly cursor?: Span | undefined
  readonly onChange: (value: string) => void
  /** Byte ranges only — the caller attaches the module, which it knows and the editor does not. */
  readonly onSelect: (range: { readonly start: number; readonly end: number }) => void
  /** Receives the format command, so a control outside the editor can trigger it. */
  readonly formatRef?: MutableRefObject<(() => boolean) | null>
  readonly className?: string
}) {
  const containerRef = useRef<HTMLDivElement | null>(null)
  const handleRef = useRef<SnippetEditor.Handle | null>(null)
  const initialRef = useRef(props.value)
  const callbacksRef = useRef({ onChange: props.onChange, onSelect: props.onSelect })
  callbacksRef.current = { onChange: props.onChange, onSelect: props.onSelect }

  useEffect(() => {
    const container = containerRef.current
    if (container === null) return
    const handle = SnippetEditor.mount({
      parent: container,
      doc: initialRef.current,
      editable: true,
      features: { diagnostics: true, hover: true },
      onChange: (value) => callbacksRef.current.onChange(value),
      onSelect: (range) => callbacksRef.current.onSelect(range),
    })
    handleRef.current = handle
    return () => {
      handleRef.current = null
      handle.destroy()
    }
  }, [])

  if (props.formatRef !== undefined)
    props.formatRef.current = () => handleRef.current?.format() ?? false

  // Module switches and preset/URL loads replace the document wholesale.
  useEffect(() => {
    handleRef.current?.setValue(props.value)
  }, [props.value])

  // The language-server session mirrors the snapshot the whole workbench shares. During a typing
  // burst the editor value is ahead of analysis; the core goes quiet until the snapshot catches up.
  useEffect(() => {
    const handle = handleRef.current
    if (handle === null) return
    const source = Analysis.sources(props.snapshot).get(props.module)
    const bytes = source === undefined ? encoder.encode(props.value) : Uint8Array.from(source.bytes)
    handle.setSession(SnippetEditor.session(props.module, bytes, props.snapshot))
    // props.value is deliberately not a dependency: sessions pair bytes with their snapshot.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [props.module, props.snapshot])

  // Reflect the shared span cursor. A cursor in a different module draws nothing here — its
  // offsets belong to another file's bytes.
  const cursor =
    props.cursor !== undefined && props.cursor.module === props.module ? props.cursor : undefined
  useEffect(() => {
    handleRef.current?.setSpanHighlight(
      cursor === undefined ? null : { start: cursor.start, end: cursor.end },
    )
  }, [cursor])

  return <div ref={containerRef} className={props.className} />
}
