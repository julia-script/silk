'use client'

import type {
  SilkAnalysisDetail,
  SilkChangeDetail,
  SilkSnippetElement,
} from '@silklang/editor-support/Element'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import { AlignLeft, Check, Clipboard, Download, LoaderCircle, TriangleAlert } from 'lucide-react'
import Link from 'next/link'
import type { DetailedHTMLProps, HTMLAttributes } from 'react'
import { useEffect, useRef, useState } from 'react'
import styles from './share.module.css'
import * as Snapshot from './snapshot'
import * as SourceCode from './SourceCode'

declare module 'react' {
  namespace JSX {
    interface IntrinsicElements {
      'silk-snippet': DetailedHTMLProps<HTMLAttributes<SilkSnippetElement>, SilkSnippetElement>
    }
  }
}

interface Options {
  readonly backdrop: boolean
  readonly diagnostics: boolean
  readonly diagnosticTooltip: boolean
  readonly filename: string
  readonly height: number | undefined
  readonly inlayHints: boolean
  readonly padding: number
  readonly width: number
}

interface Props {
  readonly initialOptions: Options
  readonly initialSource: string
  readonly monoClassName: string
}

type Action =
  | { readonly kind: 'idle'; readonly message: string }
  | { readonly kind: 'loading'; readonly message: string }
  | { readonly kind: 'success'; readonly message: string }
  | { readonly kind: 'error'; readonly message: string }

const isSilkChange = (event: Event): event is CustomEvent<SilkChangeDetail> =>
  event instanceof CustomEvent &&
  typeof event.detail === 'object' &&
  event.detail !== null &&
  'source' in event.detail

const isSilkAnalysis = (event: Event): event is CustomEvent<SilkAnalysisDetail> =>
  event instanceof CustomEvent &&
  typeof event.detail === 'object' &&
  event.detail !== null &&
  'diagnostics' in event.detail

const nextPaint = (): Promise<void> =>
  Effect.runPromise(
    Effect.callback<void>((resume) => {
      requestAnimationFrame(() => requestAnimationFrame(() => resume(Effect.void)))
    }),
  )

const diagnosticLine = (source: string, offset: number): number =>
  source.slice(0, offset).split('\n').length

const toggleAttribute = (element: HTMLElement, name: string, enabled: boolean): void => {
  if (enabled) element.setAttribute(name, '')
  else element.removeAttribute(name)
}

export function ShareWorkbench({ initialOptions, initialSource, monoClassName }: Props) {
  const snippetRef = useRef<SilkSnippetElement>(null)
  const surfaceRef = useRef<HTMLDivElement>(null)
  const resetRef = useRef<Fiber.Fiber<void>>(undefined)
  const [source, setSource] = useState(initialSource)
  const [options, setOptions] = useState(initialOptions)
  const [diagnostics, setDiagnostics] = useState<
    ReadonlyArray<{ readonly from: number; readonly message: string }>
  >([])
  const [action, setAction] = useState<Action>({
    kind: 'idle',
    message: 'Ready to export.',
  })

  const report = (next: Action): void => {
    if (resetRef.current !== undefined) Effect.runFork(Fiber.interrupt(resetRef.current))
    setAction(next)
    if (next.kind === 'success') {
      resetRef.current = Effect.runFork(
        Effect.sleep(2500).pipe(
          Effect.andThen(
            Effect.sync(() => setAction({ kind: 'idle', message: 'Ready to export.' })),
          ),
        ),
      )
    }
  }

  useEffect(
    () => () => {
      if (resetRef.current !== undefined) Effect.runFork(Fiber.interrupt(resetRef.current))
    },
    [],
  )

  useEffect(() => {
    const element = snippetRef.current
    if (element === null) return
    element.setAttribute('editable', '')
    element.setAttribute('hover', '')
    toggleAttribute(element, 'diagnostics', options.diagnostics)
    toggleAttribute(element, 'inlay-hints', options.inlayHints)

    const onChange = (event: Event) => {
      if (isSilkChange(event)) setSource(event.detail.source)
    }
    const onAnalysis = (event: Event) => {
      if (isSilkAnalysis(event)) setDiagnostics(event.detail.diagnostics)
    }
    element.addEventListener('silk-change', onChange)
    element.addEventListener('silk-analysis', onAnalysis)
    void import('@silklang/editor-support/register')

    return () => {
      element.removeEventListener('silk-change', onChange)
      element.removeEventListener('silk-analysis', onAnalysis)
    }
  }, [])

  useEffect(() => {
    const element = snippetRef.current
    if (element === null) return
    toggleAttribute(element, 'diagnostics', options.diagnostics)
    toggleAttribute(element, 'inlay-hints', options.inlayHints)
    if (!options.diagnostics) setDiagnostics([])
  }, [options.diagnostics, options.inlayHints])

  useEffect(() => {
    const update = Effect.sleep(250).pipe(
      Effect.andThen(
        Effect.sync(() => {
          const url = new URL(window.location.href)
          url.searchParams.set('code', SourceCode.encode(source))
          url.searchParams.set('hints', options.inlayHints ? '1' : '0')
          url.searchParams.set('diagnostics', options.diagnostics ? '1' : '0')
          url.searchParams.set('tooltip', options.diagnosticTooltip ? '1' : '0')
          url.searchParams.set('filename', options.filename)
          url.searchParams.set('width', String(options.width))
          url.searchParams.set('padding', String(options.padding))
          url.searchParams.set('backdrop', options.backdrop ? '1' : '0')
          if (options.height === undefined) url.searchParams.delete('height')
          else url.searchParams.set('height', String(options.height))
          window.history.replaceState(null, '', url)
        }),
      ),
    )
    const fiber = Effect.runFork(update)
    return () => {
      Effect.runFork(Fiber.interrupt(fiber))
    }
  }, [options, source])

  const updateOption = (
    name: 'backdrop' | 'diagnostics' | 'diagnosticTooltip' | 'inlayHints',
    enabled: boolean,
  ): void => {
    setOptions((current) => ({ ...current, [name]: enabled }))
  }

  const updateNumber = (
    name: 'padding' | 'width',
    value: number,
    minimum: number,
    maximum: number,
  ): void => {
    if (!Number.isFinite(value)) return
    setOptions((current) => ({
      ...current,
      [name]: Math.min(maximum, Math.max(minimum, Math.round(value))),
    }))
  }

  const format = (): void => {
    const snippet = snippetRef.current
    const formatted = snippet?.format() ?? false
    if (formatted && snippet !== null) setSource(snippet.source)
    report({
      kind: 'success',
      message: formatted ? 'Source formatted.' : 'Source is already formatted.',
    })
  }

  const prepareSnapshot = async (): Promise<Snapshot.Snapshot> => {
    const snippet = snippetRef.current
    const surface = surfaceRef.current
    if (snippet === null || surface === null) throw new Error('The editor is still loading.')
    setDiagnostics(snippet.analyze())
    await nextPaint()
    return Effect.runPromise(Snapshot.capture(surface))
  }

  const copyPng = async (): Promise<void> => {
    report({ kind: 'loading', message: 'Rendering PNG…' })
    try {
      const image = await Effect.runPromise(Snapshot.png(await prepareSnapshot()))
      if (typeof ClipboardItem === 'undefined' || navigator.clipboard.write === undefined) {
        Snapshot.download(image, 'silk-snippet.png')
        report({ kind: 'success', message: 'Clipboard unavailable; PNG downloaded.' })
        return
      }
      await navigator.clipboard.write([new ClipboardItem({ 'image/png': image })])
      report({ kind: 'success', message: 'PNG copied.' })
    } catch (cause) {
      report({
        kind: 'error',
        message: cause instanceof Error ? cause.message : 'The PNG could not be copied.',
      })
    }
  }

  const downloadSvg = async (): Promise<void> => {
    report({ kind: 'loading', message: 'Rendering SVG…' })
    try {
      const snapshot = await prepareSnapshot()
      Snapshot.download(
        new Blob([snapshot.svg], { type: 'image/svg+xml;charset=utf-8' }),
        'silk-snippet.svg',
      )
      report({ kind: 'success', message: 'SVG downloaded.' })
    } catch (cause) {
      report({
        kind: 'error',
        message: cause instanceof Error ? cause.message : 'The SVG could not be downloaded.',
      })
    }
  }

  const firstDiagnostic = options.diagnostics ? diagnostics[0] : undefined
  const tooltipTop =
    firstDiagnostic === undefined
      ? undefined
      : `${Math.min(diagnosticLine(source, firstDiagnostic.from) * 24 + 28, 260)}px`
  const busy = action.kind === 'loading'

  return (
    <div className={`${styles.share} ${monoClassName}`}>
      <header className={styles.navigation}>
        <Link className={styles.wordmark} href="/">
          silk
        </Link>
        <Link className={styles.docsLink} href="/docs/language">
          docs →
        </Link>
      </header>

      <main className={styles.main}>
        <aside className={styles.controls} aria-labelledby="share-title">
          <div className={styles.introduction}>
            <h1 id="share-title">Silk snapshot</h1>
            <p>Edit the source, choose what the compiler shows, then export the image.</p>
          </div>

          <fieldset className={styles.options}>
            <legend>Presentation</legend>
            <label className={styles.option}>
              <span>
                <strong>Inlay hints</strong>
                <small>Include inferred types.</small>
              </span>
              <input
                checked={options.inlayHints}
                onChange={(event) => updateOption('inlayHints', event.target.checked)}
                type="checkbox"
              />
            </label>
            <label className={styles.option}>
              <span>
                <strong>Diagnostics</strong>
                <small>Draw compiler squiggles.</small>
              </span>
              <input
                checked={options.diagnostics}
                onChange={(event) => updateOption('diagnostics', event.target.checked)}
                type="checkbox"
              />
            </label>
            <label className={styles.option} data-disabled={!options.diagnostics}>
              <span>
                <strong>Diagnostic tooltip</strong>
                <small>Pin the first compiler message.</small>
              </span>
              <input
                checked={options.diagnosticTooltip}
                disabled={!options.diagnostics}
                onChange={(event) => updateOption('diagnosticTooltip', event.target.checked)}
                type="checkbox"
              />
            </label>
          </fieldset>

          <fieldset className={styles.frameOptions}>
            <legend>Frame</legend>
            <label className={styles.field}>
              <span>Filename</span>
              <input
                maxLength={120}
                onChange={(event) =>
                  setOptions((current) => ({ ...current, filename: event.target.value }))
                }
                spellCheck={false}
                type="text"
                value={options.filename}
              />
            </label>
            <div className={styles.dimensions}>
              <label className={styles.field}>
                <span>Width</span>
                <input
                  inputMode="numeric"
                  max={1600}
                  min={320}
                  onChange={(event) => updateNumber('width', event.target.valueAsNumber, 320, 1600)}
                  type="number"
                  value={options.width}
                />
              </label>
              <label className={styles.field}>
                <span>Height</span>
                <input
                  inputMode="numeric"
                  max={1600}
                  min={200}
                  onChange={(event) => {
                    const value = event.target.valueAsNumber
                    setOptions((current) => ({
                      ...current,
                      height: Number.isFinite(value)
                        ? Math.min(1600, Math.max(200, Math.round(value)))
                        : undefined,
                    }))
                  }}
                  placeholder="Auto"
                  type="number"
                  value={options.height ?? ''}
                />
              </label>
            </div>
            <label className={styles.field}>
              <span>Outer padding</span>
              <input
                inputMode="numeric"
                max={160}
                min={0}
                onChange={(event) => updateNumber('padding', event.target.valueAsNumber, 0, 160)}
                type="number"
                value={options.padding}
              />
            </label>
            <label className={styles.option}>
              <span>
                <strong>Backdrop</strong>
                <small>Fill the area around the code.</small>
              </span>
              <input
                checked={options.backdrop}
                onChange={(event) => updateOption('backdrop', event.target.checked)}
                type="checkbox"
              />
            </label>
          </fieldset>

          <p className={styles.draftNote}>
            The URL follows this draft, but it is not version-pinned. Export the image you intend to
            share.
          </p>
        </aside>

        <section className={styles.workbench} aria-label="Image workbench">
          <div className={styles.actionBar}>
            <button className={styles.secondaryButton} onClick={format} type="button">
              <AlignLeft aria-hidden="true" />
              Format
            </button>
            <div className={styles.exportActions}>
              <button
                className={styles.primaryButton}
                data-state={action.kind}
                disabled={busy}
                onClick={() => void copyPng()}
                type="button"
              >
                {busy ? <LoaderCircle aria-hidden="true" /> : <Clipboard aria-hidden="true" />}
                {busy ? 'Rendering…' : 'Copy PNG'}
              </button>
              <button
                className={styles.secondaryButton}
                disabled={busy}
                onClick={() => void downloadSvg()}
                type="button"
              >
                <Download aria-hidden="true" />
                SVG
              </button>
            </div>
          </div>

          <div className={styles.previewViewport}>
            <div
              ref={surfaceRef}
              className={styles.exportSurface}
              data-backdrop={options.backdrop}
              style={{ height: options.height, padding: options.padding, width: options.width }}
            >
              <figure className={styles.codeFigure} data-snapshot-figure>
                <figcaption className={styles.codeLabel} data-snapshot-label>
                  <span>{options.filename || 'untitled.silk'}</span>
                </figcaption>
                <div
                  className={styles.codeArea}
                  data-snapshot-code
                  style={{
                    paddingBottom:
                      options.diagnosticTooltip && firstDiagnostic !== undefined ? 64 : undefined,
                  }}
                >
                  <silk-snippet ref={snippetRef} className={styles.snippet}>
                    {initialSource}
                  </silk-snippet>
                  {options.diagnosticTooltip && firstDiagnostic !== undefined ? (
                    <div
                      className={styles.diagnosticTooltip}
                      data-snapshot-diagnostic
                      style={{ top: tooltipTop }}
                    >
                      <TriangleAlert aria-hidden="true" />
                      <span>{firstDiagnostic.message}</span>
                    </div>
                  ) : null}
                </div>
              </figure>
            </div>
          </div>

          <div className={styles.status} data-state={action.kind} role="status" aria-live="polite">
            {action.kind === 'success' ? <Check aria-hidden="true" /> : null}
            {action.kind === 'error' ? <TriangleAlert aria-hidden="true" /> : null}
            {action.message}
          </div>
        </section>
      </main>

      <footer className={styles.footer}>
        <span>Silk source, rendered by the current compiler.</span>
        <span>PNG clipboard · SVG download</span>
      </footer>
    </div>
  )
}
