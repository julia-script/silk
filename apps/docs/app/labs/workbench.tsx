'use client'

/**
 * The compiler workbench: one source program, every phase available as a dockable pane.
 *
 * The ten per-phase labs each owned their own editor and rebuilt their own snapshot, so seeing
 * one program through two phases meant retyping it on two pages. Here the snapshot is built once
 * and every pane reads it, which is what makes phases comparable — a disagreement between two
 * panes is about the compiler, not about two different inputs.
 *
 * Panes are keyed by instance, so the same phase can be open twice; arrangement is dockview's
 * (drag to split, stack as tabs, resize) and lives in the URL alongside the source.
 */

import { Analysis } from '@silk-effect/compiler'
import type { ToolchainPlan } from '@silk-effect/compiler'
import type { DockviewApi, DockviewReadyEvent, IDockviewPanelProps } from 'dockview-react'
import { DockviewReact } from 'dockview-react'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { type ViewContext, viewById, views } from './registry'
import styles from './syntax-inspector/syntax-inspector.module.css'
import {
  decodeLayout,
  decodeSource,
  encodeLayout,
  encodeSource,
  layoutParam,
  sourceParam,
} from './url-state'
import shell from './workbench.module.css'

const sourceId = 'memory://docs/workbench.silk'
const encoder = new TextEncoder()

const presets = [
  {
    label: 'Nested calls',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  },
  {
    label: 'Branch diamond',
    source: 'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }',
  },
  {
    label: 'Checked arithmetic',
    source: 'pub fn main() -> I32 { return I32.divide(I32.add(40, 2), 1) }',
  },
  { label: 'Overflow traps', source: 'pub fn main() -> I32 { return I32.add(2147483647, 1) }' },
  { label: 'Unknown call', source: 'pub fn main() -> I32 { return missing() }' },
] as const

/**
 * Panes read live state through a ref rather than through panel params.
 *
 * Dockview panels are created once and outlive any particular render, so capturing the snapshot
 * in a panel's params would pin every pane to the source as it was when that pane opened. The ref
 * is written on every render and read during the pane's own render, so panes always see current
 * state without being recreated on each keystroke.
 */
interface WorkbenchState extends ViewContext {
  readonly setSource: (value: string) => void
  readonly setMode: (value: 'release' | 'debug') => void
  readonly setProfile: (value: ToolchainPlan.OptimizationProfile) => void
  /** Bumped on every state change so open panes re-render. */
  readonly version: number
}

const stateRef: { current: WorkbenchState | undefined } = { current: undefined }
const listeners = new Set<() => void>()

const subscribe = (listener: () => void): (() => void) => {
  listeners.add(listener)
  return () => listeners.delete(listener)
}

/** Re-renders every open pane. Panes are outside the React tree that owns workbench state. */
const notify = (): void => {
  for (const listener of listeners) listener()
}

function usePaneState(): WorkbenchState | undefined {
  const [, force] = useState(0)
  useEffect(() => subscribe(() => force((value) => value + 1)), [])
  return stateRef.current
}

/** The source editor, itself a pane so it can be moved and resized like any other. */
function SourcePane() {
  const state = usePaneState()
  if (state === undefined) return null

  return (
    <div className={shell.sourcePane}>
      <div className={styles.exampleBar} aria-label="Source presets">
        {presets.map((preset) => (
          <button key={preset.label} type="button" onClick={() => state.setSource(preset.source)}>
            {preset.label}
          </button>
        ))}
      </div>
      <label className="sr-only" htmlFor="workbench-source">
        Silk source code
      </label>
      <textarea
        id="workbench-source"
        className={`${styles.editor} ${shell.editorFill}`}
        value={state.source}
        onChange={(event) => state.setSource(event.target.value)}
        spellCheck={false}
        autoCapitalize="off"
        autoCorrect="off"
      />
    </div>
  )
}

/**
 * Every non-source pane. The pane's `view` parameter decides what it renders, and the header
 * picker rewrites that parameter — which is what lets one pane become any phase, and lets the
 * same phase be open in two panes at once.
 */
function ViewPane(props: IDockviewPanelProps<{ view: string }>) {
  const state = usePaneState()
  const viewId = props.params.view
  const definition = viewById(viewId)

  const onPick = useCallback(
    (next: string) => {
      const picked = viewById(next)
      if (picked === undefined) return
      props.api.updateParameters({ view: next })
      props.api.setTitle(picked.title)
    },
    [props.api],
  )

  if (state === undefined) return null

  return (
    <div className={shell.pane}>
      <div className={shell.paneBar}>
        <label className="sr-only" htmlFor={`pick-${props.api.id}`}>
          Pane content
        </label>
        <select
          id={`pick-${props.api.id}`}
          className={shell.panePicker}
          value={viewId}
          onChange={(event) => onPick(event.target.value)}
        >
          {views.map((view) => (
            <option key={view.id} value={view.id}>
              {view.title}
            </option>
          ))}
        </select>
        {definition === undefined ? null : (
          <span className={shell.panePhase}>{definition.phase}</span>
        )}
      </div>
      <div className={shell.paneBody}>
        {definition === undefined ? (
          <p className={styles.emptyState}>Unknown view: {viewId}</p>
        ) : (
          definition.render(state)
        )}
      </div>
    </div>
  )
}

const components = { source: SourcePane, view: ViewPane }

/** Opened when there is no layout in the URL and nothing saved locally. */
const defaultLayout = (api: DockviewApi): void => {
  const source = api.addPanel({ id: 'source', component: 'source', title: 'Source' })
  const mir = api.addPanel({
    id: 'pane-mir',
    component: 'view',
    title: 'MIR control flow',
    params: { view: 'mir' },
    position: { direction: 'right', referencePanel: source },
  })
  api.addPanel({
    id: 'pane-wasm',
    component: 'view',
    title: 'WebAssembly',
    params: { view: 'wasm' },
    position: { direction: 'below', referencePanel: mir },
  })
  api.addPanel({
    id: 'pane-diagnostics',
    component: 'view',
    title: 'Diagnostics',
    params: { view: 'diagnostics' },
    position: { direction: 'below', referencePanel: source },
  })
}

const storageKey = 'silk-labs-workbench-layout'

export function Workbench() {
  const [source, setSource] = useState<string>(presets[0].source)
  const [mode, setMode] = useState<'release' | 'debug'>('debug')
  const [profile, setProfile] = useState<ToolchainPlan.OptimizationProfile>('release')
  const [selectedDiagnostic, setSelectedDiagnostic] = useState<number>()
  const [api, setApi] = useState<DockviewApi>()
  const [theme, setTheme] = useState('dockview-theme-dark')
  const paneCounter = useRef(0)
  const dockRef = useRef<HTMLDivElement>(null)

  const snapshot = useMemo(() => Analysis.ofSource(sourceId, encoder.encode(source)), [source])

  // Publish current state to the panes, which live outside this component's React tree.
  stateRef.current = {
    snapshot,
    source,
    mode,
    profile,
    selectedDiagnostic,
    onSelectDiagnostic: setSelectedDiagnostic,
    setSource,
    setMode,
    setProfile,
    version: 0,
  }
  useEffect(notify)

  // Follow the site's light/dark choice, which fumadocs writes onto <html>.
  useEffect(() => {
    const sync = (): void =>
      setTheme(
        document.documentElement.classList.contains('dark')
          ? 'dockview-theme-dark'
          : 'dockview-theme-light',
      )
    sync()
    const observer = new MutationObserver(sync)
    observer.observe(document.documentElement, { attributes: true, attributeFilter: ['class'] })
    return () => observer.disconnect()
  }, [])

  /**
   * Put the theme on `.dv-shell`.
   *
   * Dockview writes its own default theme class there, so passing `className` to
   * `DockviewReact` adds a class rather than replacing the default — the old one has to come off
   * explicitly or both are present and the loser is whichever the stylesheet orders last.
   *
   * Sizing is deliberately *not* done here. Dockview already sets `height: 100%` on its wrapper
   * and shell, which is all they need; adding `flex: 1` on top sets `flex-basis: 0%`, and in a
   * column flex container that zeroes the height and beats the percentage. The container just
   * has to have a definite height of its own, which `.dock` does.
   */
  useEffect(() => {
    const shellElement = dockRef.current?.querySelector('.dv-shell')
    if (!(shellElement instanceof HTMLElement)) return
    for (const candidate of [...shellElement.classList]) {
      if (candidate.startsWith('dockview-theme-')) shellElement.classList.remove(candidate)
    }
    shellElement.classList.add(theme)
  }, [api, theme])

  const onReady = useCallback((event: DockviewReadyEvent) => {
    setApi(event.api)

    const params = new URLSearchParams(window.location.search)
    const urlSource = params.get(sourceParam)
    if (urlSource !== null) {
      const decoded = decodeSource(urlSource)
      if (decoded !== undefined) setSource(decoded)
    }

    // A URL layout wins over the saved one: an explicit link is a request for that exact view.
    const restore = async (): Promise<void> => {
      const urlLayout = params.get(layoutParam)
      if (urlLayout !== null) {
        const layout = await decodeLayout(urlLayout)
        if (layout !== undefined) {
          try {
            event.api.fromJSON(layout)
            return
          } catch {
            // Fall through to the saved or default layout.
          }
        }
      }

      const saved = window.localStorage.getItem(storageKey)
      if (saved !== null) {
        const layout = await decodeLayout(saved)
        if (layout !== undefined) {
          try {
            event.api.fromJSON(layout)
            return
          } catch {
            // Fall through to the default layout.
          }
        }
      }

      defaultLayout(event.api)
    }

    void restore()
  }, [])

  // Keep the URL in step with the layout and the source, so a refresh or a shared link lands on
  // the same view. `replaceState` rather than push: dragging a pane should not fill up history.
  useEffect(() => {
    if (api === undefined) return

    let frame = 0
    const sync = (): void => {
      cancelAnimationFrame(frame)
      // Layout events fire in bursts while dragging; one write per frame is plenty.
      frame = requestAnimationFrame(() => {
        void (async () => {
          const encoded = await encodeLayout(api.toJSON())
          if (encoded === undefined) return
          try {
            window.localStorage.setItem(storageKey, encoded)
          } catch {
            // Private-mode quota failures should not break the workbench.
          }
          const next = new URLSearchParams(window.location.search)
          next.set(layoutParam, encoded)
          next.set(sourceParam, encodeSource(stateRef.current?.source ?? ''))
          window.history.replaceState(null, '', `${window.location.pathname}?${next}`)
        })()
      })
    }

    const disposable = api.onDidLayoutChange(sync)
    sync()
    return () => {
      cancelAnimationFrame(frame)
      disposable.dispose()
    }
  }, [api, source])

  const addPane = useCallback(
    (viewId: string) => {
      if (api === undefined) return
      const definition = viewById(viewId)
      if (definition === undefined) return
      paneCounter.current += 1
      api.addPanel({
        // Instance-keyed, not phase-keyed: opening MIR twice must produce two panes.
        id: `pane-${viewId}-${paneCounter.current}`,
        component: 'view',
        title: definition.title,
        params: { view: viewId },
      })
    },
    [api],
  )

  return (
    <div className={shell.workbench}>
      <div className={shell.toolbar}>
        <h1 className={shell.toolbarTitle}>Compiler workbench</h1>

        <label className="sr-only" htmlFor="add-pane">
          Add a pane
        </label>
        <select
          id="add-pane"
          className={shell.toggle}
          value=""
          onChange={(event) => {
            addPane(event.target.value)
            event.currentTarget.value = ''
          }}
        >
          <option value="" disabled>
            + Add pane
          </option>
          {views.map((view) => (
            <option key={view.id} value={view.id}>
              {view.title}
            </option>
          ))}
        </select>

        <button
          type="button"
          className={shell.toggle}
          onClick={() => {
            if (api === undefined) return
            for (const panel of [...api.panels]) api.removePanel(panel)
            defaultLayout(api)
          }}
        >
          Reset layout
        </button>

        <span className={shell.toolbarSpacer} />

        <button
          type="button"
          className={shell.toggle}
          onClick={() => setMode(mode === 'release' ? 'debug' : 'release')}
          aria-pressed={mode === 'debug'}
        >
          {mode}
        </button>
        <label className="sr-only" htmlFor="profile">
          Optimization profile
        </label>
        <select
          id="profile"
          className={shell.toggle}
          value={profile}
          onChange={(event) =>
            setProfile(event.target.value as ToolchainPlan.OptimizationProfile)
          }
        >
          {(['debug', 'release', 'release-with-debug'] as const).map((candidate) => (
            <option key={candidate} value={candidate}>
              {candidate}
            </option>
          ))}
        </select>
      </div>

      <div className={shell.dockFrame}>
        <div className={shell.dock} ref={dockRef}>
          <DockviewReact components={components} onReady={onReady} />
        </div>
      </div>
    </div>
  )
}
