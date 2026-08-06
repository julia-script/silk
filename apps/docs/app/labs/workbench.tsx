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
import * as Snapshot from './snapshot'
import { ToolchainPlan } from '@silk-effect/compiler'
import type { BootstrapEvaluation, Target } from '@silk-effect/compiler'
import type { DockviewApi, DockviewReadyEvent, IDockviewPanelProps } from 'dockview-react'
import { DockviewReact } from 'dockview-react'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { PresetPalette } from './preset-palette'
import { type Preset, presets } from './presets'
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

const encoder = new TextEncoder()

const initialPreset = presets.find((preset) => preset.label === 'Nested calls') ?? presets[0]

/**
 * Panes read live state through a ref rather than through panel params.
 *
 * Dockview panels are created once and outlive any particular render, so capturing the snapshot
 * in a panel's params would pin every pane to the source as it was when that pane opened. The ref
 * is written on every render and read during the pane's own render, so panes always see current
 * state without being recreated on each keystroke.
 */
interface WorkbenchState extends ViewContext {
  readonly setModuleSource: (name: string, value: string) => void
  readonly setProfile: (value: ToolchainPlan.OptimizationProfile) => void
  readonly openPalette: () => void
  readonly addModule: () => void
  readonly removeModule: (name: string) => void
  readonly setRoot: (name: string) => void
  readonly activeModule: string
  readonly setActiveModule: (name: string) => void
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

/**
 * The source editor, itself a pane so it can be moved and resized like any other.
 *
 * A compilation request is a set of named modules, not one file, so the editor carries a tab per
 * module. Most programs have exactly one and the tab strip stays out of the way; module closure
 * and the declaration index only become explorable once more than one is possible.
 */
function SourcePane() {
  const state = usePaneState()
  if (state === undefined) return null

  const names = Object.keys(state.modules)
  const active = names.includes(state.activeModule) ? state.activeModule : (names[0] ?? '')

  return (
    <div className={shell.sourcePane}>
      <div className={shell.moduleBar}>
        {names.map((name) => (
          <span key={name} className={shell.moduleTab} data-active={name === active}>
            <button type="button" onClick={() => state.setActiveModule(name)}>
              {name}
              {name === state.root ? <i className={shell.rootDot} title="root module" /> : null}
            </button>
            {names.length > 1 ? (
              <button
                type="button"
                className={shell.moduleClose}
                aria-label={`Remove module ${name}`}
                onClick={() => state.removeModule(name)}
              >
                ×
              </button>
            ) : null}
          </span>
        ))}
        <button
          type="button"
          className={shell.moduleAdd}
          onClick={state.addModule}
          aria-label="Add a module"
        >
          +
        </button>
        <span className={shell.toolbarSpacer} />
        {active === state.root ? null : (
          <button type="button" className={shell.moduleAdd} onClick={() => state.setRoot(active)}>
            make root
          </button>
        )}
        <button type="button" className={shell.moduleAdd} onClick={state.openPalette}>
          presets…
        </button>
      </div>

      <label className="sr-only" htmlFor="workbench-source">
        Silk source code
      </label>
      <textarea
        id="workbench-source"
        className={`${styles.editor} ${shell.editorFill}`}
        value={state.modules[active] ?? ''}
        onChange={(event) => state.setModuleSource(active, event.target.value)}
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
          // A pane restored from an older layout carries a retired id; showing the view it
          // resolved to keeps the picker honest about what is actually rendered below.
          value={definition?.id ?? viewId}
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
    id: 'pane-backend',
    component: 'view',
    title: 'Backend output',
    params: { view: 'backend' },
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
  const [modules, setModules] = useState<Readonly<Record<string, string>>>(initialPreset.modules)
  const [root, setRoot] = useState<string>(initialPreset.root)
  const [activeModule, setActiveModule] = useState<string>(initialPreset.root)
  // One optimization profile drives both panes. Codegen's debug-info mode is *derived* from it
  // rather than being its own control: the profile already says whether debug info is wanted
  // (`-g`), so a separate toggle only adds states where the two disagree — the backend pane
  // showing stripped IR for a build the toolchain plans with `-g`.
  const [profile, setProfile] = useState<ToolchainPlan.OptimizationProfile>('release')
  const mode = ToolchainPlan.codegenModeFor(profile)
  // The target belongs to the request, not to a backend: the same program lowers differently for
  // each, which is exactly the comparison the layout and MIR panes are for.
  const [target, setTarget] = useState<Target.Id>('aarch64-apple-darwin')
  const [selectedDiagnostic, setSelectedDiagnostic] = useState<number>()
  const [selectedFlowId, setSelectedFlowId] = useState<string>()
  const [evaluation, setEvaluation] = useState<BootstrapEvaluation.Outcome>()
  const [paletteOpen, setPaletteOpen] = useState(false)
  const [api, setApi] = useState<DockviewApi>()
  const [theme, setTheme] = useState('dockview-theme-dark')
  const paneCounter = useRef(0)
  const dockRef = useRef<HTMLDivElement>(null)

  const snapshot = useMemo(
    () =>
      Snapshot.make({
        rootModule: root,
        sources: new Map(
          Object.entries(modules).map(([name, text]) => [name, encoder.encode(text)]),
        ),
        target,
      }),
    [modules, root, target],
  )

  // An evaluation describes the program that produced it, so it cannot outlive that program: a
  // trace still on screen after an edit would be claiming results for source that no longer
  // exists. Keying on the snapshot catches every path that changes it — preset, edit, module
  // add/remove, target — rather than relying on each one to remember.
  // biome-ignore lint/correctness/useExhaustiveDependencies: the snapshot IS the dependency here
  useEffect(() => {
    setEvaluation(undefined)
    setSelectedFlowId(undefined)
  }, [snapshot])

  const loadPreset = useCallback((preset: Preset) => {
    setModules(preset.modules)
    setRoot(preset.root)
    setActiveModule(preset.root)
    setSelectedDiagnostic(undefined)
  }, [])

  const addModule = useCallback(() => {
    setModules((current) => {
      let index = current.lib === undefined ? 0 : 2
      let name = index === 0 ? 'lib' : `lib${index}`
      while (current[name] !== undefined) {
        index += 1
        name = `lib${index}`
      }
      setActiveModule(name)
      return { ...current, [name]: 'pub fn answer() -> I32 { return 1 }' }
    })
  }, [])

  const removeModule = useCallback(
    (name: string) => {
      setModules((current) => {
        const { [name]: _removed, ...rest } = current
        const remaining = Object.keys(rest)
        if (remaining.length === 0) return current
        // Removing the root would leave the request pointing at a module that is not there, so
        // the root moves to whatever is left rather than the request becoming unloadable.
        if (name === root) setRoot(remaining[0] as string)
        setActiveModule((activeName) =>
          activeName === name ? (remaining[0] as string) : activeName,
        )
        return rest
      })
    },
    [root],
  )

  // Publish current state to the panes, which live outside this component's React tree.
  stateRef.current = {
    snapshot,
    modules,
    root,
    mode,
    profile,
    selectedDiagnostic,
    onSelectDiagnostic: setSelectedDiagnostic,
    selectedFlowId,
    onSelectFlow: setSelectedFlowId,
    evaluation,
    // Evaluation runs the lowered MIR, which is absent when the target did not resolve.
    onEvaluate: () =>
      setEvaluation(
        Analysis.mirOf(snapshot)._tag === 'Available' ? Analysis.evaluate(snapshot) : undefined,
      ),
    setModuleSource: (name, value) => setModules((current) => ({ ...current, [name]: value })),
    setProfile,
    openPalette: () => setPaletteOpen(true),
    addModule,
    removeModule,
    setRoot,
    activeModule,
    setActiveModule,
  }
  useEffect(notify)

  // A palette anywhere in the page, so a preset is reachable without hunting for the source pane.
  //
  // Not ⌘K: fumadocs binds that to the docs search, and this page still carries the site nav.
  // ⌘P leaves that alone and matches the "go to file" muscle memory from editors.
  useEffect(() => {
    const onKey = (event: KeyboardEvent): void => {
      if (event.key === 'p' && (event.metaKey || event.ctrlKey)) {
        event.preventDefault()
        setPaletteOpen((current) => !current)
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [])

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

    // A URL layout wins over the saved one: an explicit link is a request for that exact view.
    const restore = async (): Promise<void> => {
      const urlSource = params.get(sourceParam)
      if (urlSource !== null) {
        const decoded = await decodeSource(urlSource)
        if (decoded !== undefined) {
          setModules(decoded.modules)
          setRoot(decoded.root)
          setActiveModule(decoded.root)
          if (decoded.target !== undefined) setTarget(decoded.target as Target.Id)
        }
      }

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
          const encodedLayout = await encodeLayout(api.toJSON())
          if (encodedLayout === undefined) return
          try {
            window.localStorage.setItem(storageKey, encodedLayout)
          } catch {
            // Private-mode quota failures should not break the workbench.
          }
          const encodedSource = await encodeSource({ root, modules, target })
          const next = new URLSearchParams(window.location.search)
          next.set(layoutParam, encodedLayout)
          if (encodedSource !== undefined) next.set(sourceParam, encodedSource)
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
  }, [api, modules, root, target])

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

        <button type="button" className={shell.toggle} onClick={() => setPaletteOpen(true)}>
          Presets… <kbd className={shell.kbd}>⌘P</kbd>
        </button>

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

        <label className="sr-only" htmlFor="target">
          Compilation target
        </label>
        <select
          id="target"
          className={shell.toggle}
          value={target}
          onChange={(event) => setTarget(event.target.value as Target.Id)}
        >
          {(
            [
              'aarch64-apple-darwin',
              'x86_64-unknown-linux-gnu',
              'aarch64-unknown-linux-gnu',
              'wasm32-unknown-unknown',
            ] as const
          ).map((candidate) => (
            <option key={candidate} value={candidate}>
              {candidate}
            </option>
          ))}
        </select>

        <label className="sr-only" htmlFor="profile">
          Optimization profile
        </label>
        <select
          id="profile"
          className={shell.toggle}
          value={profile}
          onChange={(event) => setProfile(event.target.value as ToolchainPlan.OptimizationProfile)}
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

      <PresetPalette
        open={paletteOpen}
        onClose={() => setPaletteOpen(false)}
        onPick={loadPreset}
      />
    </div>
  )
}
