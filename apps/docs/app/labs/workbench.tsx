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
 *
 * Chrome is deliberately thin. The app bar is 32px, a pane header is 22px and a row is 17px,
 * because the thing that limits how many phases you can see at once is chrome, not screen.
 */

import { Analysis, ToolchainPlan } from '@silk-effect/compiler'
import type { BootstrapEvaluation, Target } from '@silk-effect/compiler'
import type { DockviewApi, DockviewReadyEvent, IDockviewPanelProps } from 'dockview-react'
import { DockviewReact } from 'dockview-react'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { PresetPalette } from './preset-palette'
import { type Preset, presetGroups, presets } from './presets'
import { type ViewContext, siblingsOf, viewById, views } from './registry'
import { EmptyState, RowList, type Span } from './row/row'
import { diagnosticCounts, diagnosticEntries, hirContract } from './row/project-syntax'
import {
  decodeLayout,
  decodeSource,
  encodeLayout,
  encodeSource,
  layoutParam,
  sourceParam,
} from './url-state'
import shell from './workbench.module.css'
import {
  decodeWorkspaces,
  seededWorkspaces,
  slotOrder,
  type Workspace,
  workspaceStorageKey,
} from './workspaces'

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
interface WorkbenchState extends Omit<ViewContext, 'filter' | 'showTrivia'> {
  readonly setModuleSource: (name: string, value: string) => void
  readonly addModule: () => void
  readonly setRoot: (name: string) => void
  readonly activeModule: string
  readonly setActiveModule: (name: string) => void
  readonly addPane: (viewId: string) => void
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

/** The phase picker: a 214px menu of all 16 views, opened from the pane's own title. */
function PhasePicker({
  active,
  onPick,
  onClose,
}: {
  readonly active: string
  readonly onPick: (id: string) => void
  readonly onClose: () => void
}) {
  return (
    <div className={shell.pickerMenu} role="menu">
      {views.map((view) => (
        <button
          key={view.id}
          type="button"
          role="menuitem"
          className={shell.pickerOption}
          data-active={view.id === active}
          onClick={() => {
            onPick(view.id)
            onClose()
          }}
        >
          <span className={shell.pickerTag}>{view.tag}</span>
          <span className={shell.pickerTitle}>{view.title}</span>
          <span className={shell.pickerPhase}>{view.phase}</span>
        </button>
      ))}
    </div>
  )
}

/**
 * The source editor, itself a pane so it can be moved and resized like any other.
 *
 * Modules used to be a tab strip inside this pane, which stopped scaling at about four; they now
 * live in the sidebar, so this pane is only the editor and its 19px footer.
 */
function SourceBody({ state }: { readonly state: WorkbenchState }) {
  const names = Object.keys(state.modules)
  const active = names.includes(state.activeModule) ? state.activeModule : (names[0] ?? '')
  const text = state.modules[active] ?? ''
  const cursor = state.cursor

  return (
    <>
      <label className="sr-only" htmlFor="workbench-source">
        Silk source code
      </label>
      <textarea
        id="workbench-source"
        className={shell.editor}
        value={text}
        onChange={(event) => state.setModuleSource(active, event.target.value)}
        onSelect={(event) => {
          // The editor is a phase like any other: selecting text moves the same span cursor a
          // row click moves, so a selection lights up every downstream pane.
          const target = event.currentTarget
          if (target.selectionStart === target.selectionEnd) return
          state.onSelectSpan({ start: target.selectionStart, end: target.selectionEnd })
        }}
        spellCheck={false}
        autoCapitalize="off"
        autoCorrect="off"
      />
      <div className={shell.sourceFooter}>
        <span>{active}</span>
        <span>{text.length} B</span>
        <span className={shell.spacer} />
        <span>{cursor === undefined ? 'no selection' : `sel [${cursor.start}, ${cursor.end})`}</span>
      </div>
    </>
  )
}

/**
 * Every pane. One 22px bar carries the phase picker, sibling phases, any pane-local control and
 * the pane's meta; the body is either the editor or a list of rows.
 */
function ViewPane(props: IDockviewPanelProps<{ view: string }>) {
  const state = usePaneState()
  const [pickerOpen, setPickerOpen] = useState(false)
  const [filter, setFilter] = useState('')
  const [showTrivia, setShowTrivia] = useState(false)
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

  if (definition === undefined) {
    return (
      <div className={shell.pane}>
        <div className={shell.paneBar}>
          <span className={shell.paneMeta}>Unknown view: {viewId}</span>
        </div>
      </div>
    )
  }

  const isSource = definition.id === 'source'
  const result = isSource
    ? undefined
    : definition.project({ ...state, filter, showTrivia })
  const siblings = siblingsOf(definition)

  return (
    <div className={shell.pane}>
      <div className={shell.paneBar}>
        <button
          type="button"
          className={shell.panePicker}
          data-open={pickerOpen}
          aria-haspopup="menu"
          aria-expanded={pickerOpen}
          onClick={() => setPickerOpen((open) => !open)}
        >
          <i className={shell.paneDot} aria-hidden="true" />
          <span>{definition.title}</span>
          <span className={shell.paneCaret} aria-hidden="true">
            ▾
          </span>
        </button>

        {siblings.map((sibling) => (
          <button
            key={sibling.id}
            type="button"
            className={shell.paneTab}
            onClick={() => onPick(sibling.id)}
          >
            {sibling.title}
          </button>
        ))}

        {definition.hasFilter === true ? (
          <>
            <label className="sr-only" htmlFor={`filter-${props.api.id}`}>
              Filter rows
            </label>
            {/* Recessed well + glyph: what separates a field from the buttons beside it. */}
            <span className={shell.filterWell}>
              <span className={shell.filterGlyph} aria-hidden="true">
                ⌕
              </span>
              <input
                id={`filter-${props.api.id}`}
                className={shell.paneFilter}
                value={filter}
                onChange={(event) => setFilter(event.target.value)}
                placeholder="filter…"
              />
            </span>
            <button
              type="button"
              className={shell.paneToggle}
              aria-pressed={showTrivia}
              onClick={() => setShowTrivia((current) => !current)}
            >
              trivia
            </button>
          </>
        ) : null}

        {definition.action === undefined ? null : (
          <button
            type="button"
            className={shell.paneToggle}
            onClick={() => definition.action?.run({ ...state, filter, showTrivia })}
          >
            {definition.action.label}
          </button>
        )}

        <span className={shell.spacer} />
        <span className={shell.paneMeta}>{result?.meta ?? ''}</span>
      </div>

      {result?.facts === undefined ? null : (
        <div className={shell.factStrip}>
          {result.facts.map((fact) => (
            <span key={fact.text} data-tone={fact.tone ?? 'default'}>
              {fact.text}
            </span>
          ))}
        </div>
      )}

      {isSource ? (
        <SourceBody state={state} />
      ) : result?.unavailable !== undefined ? (
        <EmptyState reason>{result.unavailable}</EmptyState>
      ) : (
        <RowList
          rows={result?.rows ?? []}
          cursor={state.cursor}
          label={`${definition.title} rows`}
        />
      )}

      {pickerOpen ? (
        <PhasePicker active={definition.id} onPick={onPick} onClose={() => setPickerOpen(false)} />
      ) : null}
    </div>
  )
}

const components = { source: ViewPane, view: ViewPane }

/**
 * Opens a workspace's six panes as three columns of two.
 *
 * Order matters. Splitting a pane downward and *then* splitting the result rightward nests the
 * new column inside the first column's row, and the six panes come out at wildly different sizes
 * — dockview sizes each split relative to the panel it was given, so the errors compound.
 * Building the three columns first and only then splitting each one downward keeps the tree two
 * levels deep, which is the shape the 30/35/35 × 56/44 proportions can actually be applied to.
 */
const openWorkspace = (api: DockviewApi, workspace: Workspace): void => {
  for (const panel of [...api.panels]) api.removePanel(panel)

  const [a1, a2, b1, b2, c1, c2] = slotOrder.map((slot) => workspace.panes[slot])

  const columnA = api.addPanel({
    id: 'pane-a1',
    component: 'view',
    title: viewById(a1 as string)?.title ?? 'Source',
    params: { view: a1 },
  })
  const columnB = api.addPanel({
    id: 'pane-b1',
    component: 'view',
    title: viewById(b1 as string)?.title ?? '',
    params: { view: b1 },
    position: { direction: 'right', referencePanel: columnA },
  })
  const columnC = api.addPanel({
    id: 'pane-c1',
    component: 'view',
    title: viewById(c1 as string)?.title ?? '',
    params: { view: c1 },
    position: { direction: 'right', referencePanel: columnB },
  })

  api.addPanel({
    id: 'pane-a2',
    component: 'view',
    title: viewById(a2 as string)?.title ?? '',
    params: { view: a2 },
    position: { direction: 'below', referencePanel: columnA },
  })
  api.addPanel({
    id: 'pane-b2',
    component: 'view',
    title: viewById(b2 as string)?.title ?? '',
    params: { view: b2 },
    position: { direction: 'below', referencePanel: columnB },
  })
  api.addPanel({
    id: 'pane-c2',
    component: 'view',
    title: viewById(c2 as string)?.title ?? '',
    params: { view: c2 },
    position: { direction: 'below', referencePanel: columnC },
  })

  // Proportions are applied after the tree exists, because a split can only divide the space its
  // reference panel already had — asking for 30/35/35 while building would be measured against
  // whatever the previous split happened to leave.
  const width = api.width
  const height = api.height
  if (width > 0 && height > 0) {
    const top = Math.round(height * 0.56)
    columnA.api.setSize({ width: Math.round(width * 0.3), height: top })
    columnB.api.setSize({ width: Math.round(width * 0.35), height: top })
    columnC.api.setSize({ width: Math.round(width * 0.35), height: top })
  }
}

const storageKey = 'silk-labs-workbench-layout'

const targets = [
  'aarch64-apple-darwin',
  'x86_64-unknown-linux-gnu',
  'aarch64-unknown-linux-gnu',
  'wasm32-unknown-unknown',
] as const

const profiles = ['debug', 'release', 'release-with-debug'] as const

export function Workbench() {
  const [modules, setModules] = useState<Readonly<Record<string, string>>>(initialPreset.modules)
  const [root, setRoot] = useState<string>(initialPreset.root)
  const [activeModule, setActiveModule] = useState<string>(initialPreset.root)
  const [programName, setProgramName] = useState<string>(initialPreset.label)
  // One optimization profile drives both panes. Codegen's debug-info mode is *derived* from it
  // rather than being its own control: the profile already says whether debug info is wanted
  // (`-g`), so a separate toggle only adds states where the two disagree — the backend pane
  // showing stripped IR for a build the toolchain plans with `-g`.
  const [profile, setProfile] = useState<ToolchainPlan.OptimizationProfile>('release')
  const mode = ToolchainPlan.codegenModeFor(profile)
  // The target belongs to the request, not to a backend: the same program lowers differently for
  // each, which is exactly the comparison the layout and MIR panes are for.
  const [target, setTarget] = useState<Target.Id>('aarch64-apple-darwin')
  const [cursor, setCursor] = useState<Span>()
  const [evaluation, setEvaluation] = useState<BootstrapEvaluation.Outcome>()
  const [paletteOpen, setPaletteOpen] = useState(false)
  const [sidebarOpen, setSidebarOpen] = useState(true)
  const [namingWorkspace, setNamingWorkspace] = useState(false)
  // Which project groups are expanded in the sidebar. Collapsed by default: 98 programs is a
  // list to opt into, not to scroll past.
  const [openGroups, setOpenGroups] = useState<ReadonlySet<string>>(new Set())
  const [api, setApi] = useState<DockviewApi>()
  const [workspaces, setWorkspaces] = useState<ReadonlyArray<Workspace>>(seededWorkspaces)
  const [activeWorkspace, setActiveWorkspace] = useState<string>('Backend triage')
  const paneCounter = useRef(0)
  const dockRef = useRef<HTMLDivElement>(null)

  const snapshot = useMemo(
    () =>
      Analysis.make({
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
    setCursor(undefined)
  }, [snapshot])

  const analysis = useMemo(() => Analysis.rootAnalysis(snapshot), [snapshot])
  const counts = useMemo(
    () =>
      diagnosticCounts(
        diagnosticEntries(
          analysis.syntax.lexicalDiagnostics,
          analysis.syntax.parserDiagnostics,
          analysis.diagnostics,
        ),
      ),
    [analysis],
  )

  const loadPreset = useCallback((preset: Preset) => {
    setModules(preset.modules)
    setRoot(preset.root)
    setActiveModule(preset.root)
    setProgramName(preset.label)
    setCursor(undefined)
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

  // Publish current state to the panes, which live outside this component's React tree.
  stateRef.current = {
    snapshot,
    modules,
    root,
    mode,
    profile,
    cursor,
    onSelectSpan: setCursor,
    evaluation,
    // Evaluation runs the lowered MIR, which is absent when the target did not resolve.
    onEvaluate: () =>
      setEvaluation(
        Analysis.mirOf(snapshot)._tag === 'Available' ? Analysis.evaluate(snapshot) : undefined,
      ),
    setModuleSource: (name, value) => setModules((current) => ({ ...current, [name]: value })),
    addModule,
    setRoot,
    activeModule,
    setActiveModule,
    addPane,
  }
  useEffect(notify)

  // ⌘P for the program palette, ⌘K for the pane palette. ⌘K used to belong to fumadocs' docs
  // search, which this page no longer carries.
  useEffect(() => {
    const onKey = (event: KeyboardEvent): void => {
      if (!(event.metaKey || event.ctrlKey)) return
      if (event.key === 'p') {
        event.preventDefault()
        setPaletteOpen((current) => !current)
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [])

  // Restore saved workspaces once, on mount.
  useEffect(() => {
    const saved = decodeWorkspaces(window.localStorage.getItem(workspaceStorageKey))
    if (saved.length > 0) setWorkspaces([...seededWorkspaces, ...saved])
  }, [])

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
            // Fall through to the default workspace.
          }
        }
      }

      const seeded = seededWorkspaces.find((entry) => entry.name === 'Backend triage')
      if (seeded !== undefined) openWorkspace(event.api, seeded)
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

  /** The span cursor read through every phase — the join that links the panes. */
  const trail = useMemo(() => {
    if (cursor === undefined) return []
    const source = modules[root] ?? ''
    const slice = source.slice(cursor.start, cursor.end)
    const cells: Array<{ phase: string; value: string; missing?: boolean }> = [
      { phase: 'src', value: slice.trim() === '' ? '—' : slice.trim() },
    ]

    const token = analysis.syntax.tokens.find(
      (candidate) =>
        candidate.span.start <= cursor.start && candidate.span.end >= cursor.end,
    )
    cells.push({ phase: 'cst', value: token?.kind ?? 'no token', missing: token === undefined })

    const fn = analysis.hir.functions.find(
      (candidate) =>
        candidate.declaration.syntax.span.start <= cursor.start &&
        candidate.declaration.syntax.span.end >= cursor.end,
    )
    cells.push({
      phase: 'hir',
      value:
        fn === undefined
          ? 'not elaborated'
          : `fn#${fn.declaration.id.ordinal} ${hirContract(fn.contract)}`,
      missing: fn === undefined,
    })

    const mir = Analysis.mirOf(snapshot)
    cells.push({
      phase: 'mir',
      value:
        mir._tag === 'Available'
          ? (mir.value.functions.find((candidate) =>
              candidate.blocks.some((block) =>
                block.operations.some(
                  (operation) =>
                    operation.provenance.span.start <= cursor.start &&
                    operation.provenance.span.end >= cursor.end,
                ),
              ),
            )?.id.name ?? 'no operation')
          : 'unavailable',
      missing: mir._tag !== 'Available',
    })

    return cells
  }, [cursor, modules, root, analysis, snapshot])

  // The name comes from an inline input rather than `window.prompt`: embedded webviews block
  // prompt() outright, and a native dialog would be the only modal on a page that has none.
  const saveWorkspace = useCallback((name: string) => {
    if (api === undefined || name.trim() === '') return
    // A workspace is the *arrangement*, so it stores the views its panes currently show.
    const panes = api.panels.slice(0, 6)
    const entry: Workspace = {
      name: name.trim(),
      panes: Object.fromEntries(
        slotOrder.map((slot, index) => [
          slot,
          (panes[index]?.params as { view?: string } | undefined)?.view ?? 'source',
        ]),
      ) as Workspace['panes'],
    }
    setWorkspaces((current) => {
      const next = [...current.filter((candidate) => candidate.name !== entry.name), entry]
      try {
        window.localStorage.setItem(
          workspaceStorageKey,
          JSON.stringify(next.filter((candidate) => !seededWorkspaces.includes(candidate))),
        )
      } catch {
        // Private-mode quota failures should not break the workbench.
      }
      return next
    })
    setActiveWorkspace(entry.name)
  }, [api])

  const moduleNames = Object.keys(modules)

  return (
    <div className={`${shell.workbench} workbenchRoot`}>
      <div className={shell.appBar}>
        <button
          type="button"
          className={shell.sidebarToggle}
          onClick={() => setSidebarOpen((open) => !open)}
          title={sidebarOpen ? 'Hide the program sidebar' : 'Show the program sidebar'}
          aria-expanded={sidebarOpen}
        >
          {sidebarOpen ? '‹' : '›'}
        </button>
        <span className={shell.wordmark}>silk</span>

        <div className={shell.workspaces}>
          {workspaces.map((workspace) => (
            <button
              key={workspace.name}
              type="button"
              className={shell.workspace}
              data-active={workspace.name === activeWorkspace}
              onClick={() => {
                if (api === undefined) return
                openWorkspace(api, workspace)
                setActiveWorkspace(workspace.name)
              }}
            >
              {workspace.name}
            </button>
          ))}
          {namingWorkspace ? (
            <input
              className={shell.workspaceInput}
              placeholder="workspace name…"
              aria-label="Name for the saved workspace"
              // The input replaces the `+` the user just clicked, so focus belongs here.
              // biome-ignore lint/a11y/noAutofocus: appears on explicit click, in place of the trigger
              autoFocus
              onKeyDown={(event) => {
                if (event.key === 'Enter') {
                  saveWorkspace(event.currentTarget.value)
                  setNamingWorkspace(false)
                }
                if (event.key === 'Escape') setNamingWorkspace(false)
              }}
              onBlur={() => setNamingWorkspace(false)}
            />
          ) : (
            <button
              type="button"
              className={shell.workspace}
              onClick={() => setNamingWorkspace(true)}
              title="Save this arrangement as a workspace"
            >
              +
            </button>
          )}
        </div>

        <label className="sr-only" htmlFor="add-pane">
          Add a pane
        </label>
        <select
          id="add-pane"
          className={shell.barButton}
          value=""
          onChange={(event) => {
            addPane(event.target.value)
            event.currentTarget.value = ''
          }}
        >
          <option value="" disabled>
            + pane
          </option>
          {views.map((view) => (
            <option key={view.id} value={view.id}>
              {view.title}
            </option>
          ))}
        </select>

        <button
          type="button"
          className={shell.barButton}
          title="Reset to this workspace's arrangement"
          onClick={() => {
            const workspace = workspaces.find((entry) => entry.name === activeWorkspace)
            if (api !== undefined && workspace !== undefined) openWorkspace(api, workspace)
          }}
        >
          ↺
        </button>

        <span className={shell.spacer} />

        <span className={shell.barField}>
          <span className={shell.eyebrow}>target</span>
          <label className="sr-only" htmlFor="target">
            Compilation target
          </label>
          <select
            id="target"
            className={shell.barSelect}
            value={target}
            onChange={(event) => setTarget(event.target.value as Target.Id)}
          >
            {targets.map((candidate) => (
              <option key={candidate} value={candidate}>
                {candidate}
              </option>
            ))}
          </select>
        </span>

        <span className={shell.barField}>
          <span className={shell.eyebrow}>profile</span>
          <label className="sr-only" htmlFor="profile">
            Optimization profile
          </label>
          <select
            id="profile"
            className={shell.barSelect}
            value={profile}
            onChange={(event) =>
              setProfile(event.target.value as ToolchainPlan.OptimizationProfile)
            }
          >
            {profiles.map((candidate) => (
              <option key={candidate} value={candidate}>
                {candidate}
              </option>
            ))}
          </select>
        </span>

        <div className={shell.health}>
          <span className={shell.healthItem} data-state={counts.errors === 0 ? 'ok' : 'error'}>
            <i className={shell.healthDot} aria-hidden="true" />
            {counts.errors} err
          </span>
          <span className={shell.healthItem}>{analysis.hir.functions.length} fn</span>
        </div>
      </div>

      <div className={shell.body}>
        {sidebarOpen ? (
          <div className={shell.sidebar}>
            <div className={shell.sidebarHeader}>
              <span className={shell.eyebrow}>program</span>
              <span className={shell.spacer} />
              <button
                type="button"
                className={shell.sidebarButton}
                onClick={addModule}
                title="Add a module"
              >
                +
              </button>
              <button
                type="button"
                className={shell.sidebarButton}
                onClick={() => setSidebarOpen(false)}
                title="Hide sidebar"
              >
                ‹
              </button>
            </div>
            <div className={shell.sidebarScroll}>
              <div className={shell.sidebarSection}>
                modules<span className={shell.sidebarCount}>{moduleNames.length}</span>
              </div>
              {moduleNames.map((name) => (
                <button
                  key={name}
                  type="button"
                  className={shell.sidebarRow}
                  data-active={name === activeModule}
                  onClick={() => setActiveModule(name)}
                >
                  <span className={shell.sidebarRowName}>{name}</span>
                  {name === root ? <i className={shell.rootDot} title="root module" /> : null}
                  <span className={shell.sidebarRowMeta}>
                    {(modules[name] ?? '').length} B
                  </span>
                </button>
              ))}

              <div className={`${shell.sidebarSection} ${shell.sidebarDivider}`}>
                {/* "projects", not "presets": the shipped programs are the first residents, and
                    user-authored code will live under the same header. */}
                projects<span className={shell.sidebarCount}>{presets.length}</span>
              </div>
              {presetGroups.map(([group, entries]) => {
                const open = openGroups.has(group)
                return (
                  <div key={group}>
                    <button
                      type="button"
                      className={shell.sidebarRow}
                      aria-expanded={open}
                      onClick={() =>
                        setOpenGroups((current) => {
                          const next = new Set(current)
                          if (open) next.delete(group)
                          else next.add(group)
                          return next
                        })
                      }
                    >
                      <span className={shell.sidebarCaret} aria-hidden="true">
                        {open ? '▾' : '▸'}
                      </span>
                      <span className={shell.sidebarRowName}>{group}</span>
                      <span className={shell.sidebarRowMeta}>{entries.length}</span>
                    </button>
                    {open
                      ? entries.map((preset) => (
                          <button
                            key={preset.label}
                            type="button"
                            className={shell.sidebarRow}
                            data-active={preset.label === programName}
                            onClick={() => loadPreset(preset)}
                          >
                            <span className={shell.sidebarChildIndent} aria-hidden="true" />
                            <span className={shell.sidebarRowName}>{preset.label}</span>
                          </button>
                        ))
                      : null}
                  </div>
                )
              })}
            </div>
          </div>
        ) : null}

        <div className={shell.dockFrame}>
          <div className={shell.dock} ref={dockRef}>
            <DockviewReact components={components} onReady={onReady} />
          </div>
        </div>
      </div>

      <div className={shell.spanBar}>
        <span className={shell.spanCursor}>
          <i className={shell.spanDot} aria-hidden="true" />
          <span className={shell.eyebrow}>span</span>
          <span className={shell.spanValue}>
            {cursor === undefined ? '—' : `[${cursor.start}, ${cursor.end})`}
          </span>
        </span>
        {trail.map((cell) => (
          <span key={cell.phase} className={shell.spanCell}>
            <span className={shell.spanCellPhase}>{cell.phase}</span>
            <span
              className={shell.spanCellValue}
              data-tone={cell.missing === true ? 'missing' : undefined}
            >
              {cell.value}
            </span>
          </span>
        ))}
        <span className={shell.spacer} />
        {cursor === undefined ? (
          <span className={shell.spanHint}>click any row in any pane to move the span cursor</span>
        ) : null}
      </div>

      <div className={shell.statusBar}>
        <span className={shell.statusCell} data-tone={counts.errors === 0 ? 'ok' : 'error'}>
          {counts.errors === 0 ? 'syntax ok' : `${counts.errors} error`}
        </span>
        <span className={shell.statusCell}>{analysis.hir.functions.length} fn</span>
        <span className={shell.statusCell}>{moduleNames.length} mod</span>
        <span className={shell.spacer} />
        <span className={shell.statusEnd}>layout · {activeWorkspace}</span>
        <span className={shell.statusEnd}>url synced</span>
      </div>

      <PresetPalette
        open={paletteOpen}
        onClose={() => setPaletteOpen(false)}
        onPick={loadPreset}
      />
    </div>
  )
}
