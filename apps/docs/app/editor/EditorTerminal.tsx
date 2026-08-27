'use client'

import { useAtomSet, useAtomSubscribe, useAtomValue } from '@effect/atom-react'
import * as WebContainerError from '@silklang/platform-webcontainer/WebContainerError'
import * as Result from 'effect/Result'
import { AsyncResult } from 'effect/unstable/reactivity'
import { useCallback, useEffect, useRef, useState } from 'react'
import styles from './editor.module.css'
import * as TerminalOutput from './TerminalOutput'
import * as TerminalRenderer from './TerminalRenderer'
import * as TerminalSession from './TerminalSession'

const failureMessage = (failure: unknown): string =>
  WebContainerError.messageOf(failure) ??
  (failure instanceof Error ? failure.message : 'An unexpected terminal failure occurred')

export function EditorTerminal() {
  const hostRef = useRef<HTMLDivElement>(null)
  const rendererRef = useRef<TerminalRenderer.TerminalRenderer>(null)
  const session = useAtomValue(TerminalSession.sessionAtom)
  const writeInput = useAtomSet(TerminalSession.writeAtom)
  const resize = useAtomSet(TerminalSession.resizeAtom)
  const [rendererFailure, setRendererFailure] = useState<string>()
  const [outputFailure, setOutputFailure] = useState<string>()

  const handleOutput = useCallback((result: AsyncResult.AsyncResult<string, unknown>) => {
    TerminalOutput.consume(result, {
      onFailure: (failure) => setOutputFailure(failureMessage(failure)),
      onSuccess: (value) => rendererRef.current?.write(value),
    })
  }, [])

  useEffect(() => {
    const host = hostRef.current
    if (host === null) {
      return
    }

    const renderer = TerminalRenderer.make(host, {
      onInput: writeInput,
      onResize: resize,
      onError: (error) => setRendererFailure(error.message),
    })
    if (Result.isFailure(renderer)) {
      setRendererFailure(renderer.failure.message)
      return
    }

    rendererRef.current = renderer.success
    return () => {
      rendererRef.current = null
      renderer.success.dispose()
    }
  }, [resize, writeInput])

  useAtomSubscribe(TerminalSession.outputAtom, handleOutput, { immediate: true })

  const sessionStatus = AsyncResult.matchWithWaiting(session, {
    onWaiting: () => ({ kind: 'waiting' as const, message: 'Booting the browser workspace…' }),
    onError: (error) => ({ kind: 'failure' as const, message: failureMessage(error) }),
    onDefect: (defect) => ({ kind: 'failure' as const, message: failureMessage(defect) }),
    onSuccess: () => ({ kind: 'ready' as const }),
  })
  const sessionFailure = sessionStatus.kind === 'failure' ? sessionStatus.message : undefined
  const failure = rendererFailure ?? outputFailure ?? sessionFailure

  return (
    <main className={styles.editor} aria-label="Silk editor">
      <header className={styles.header}>
        <div>
          <span className={styles.eyebrow}>Silk workspace</span>
          <h1 className={styles.title}>Editor</h1>
        </div>
        <span className={styles.status} data-state={sessionStatus.kind}>
          {sessionStatus.kind === 'ready' ? 'WebContainer ready' : sessionStatus.message}
        </span>
      </header>

      <section className={styles.workspace} aria-label="Editor workspace">
        <div className={styles.terminalPanel}>
          <div ref={hostRef} className={styles.terminal} data-testid="terminal-host" />
          {sessionStatus.kind === 'waiting' ? (
            <div className={styles.overlay} role="status">
              {sessionStatus.message}
            </div>
          ) : null}
          {failure !== undefined ? (
            <div className={styles.error} role="alert">
              <strong>Terminal unavailable</strong>
              <span>{failure}</span>
              <span>Reload the page after confirming this route is cross-origin isolated.</span>
            </div>
          ) : null}
        </div>
      </section>
    </main>
  )
}
