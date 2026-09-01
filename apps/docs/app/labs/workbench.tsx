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
 * State lives in atoms (see state.ts): panes are portaled out of this component's tree by
 * dockview, and atoms are addressable from anywhere under the RegistryProvider, so each pane
 * subscribes to exactly the state it reads.
 *
 * Chrome is deliberately thin. The app bar is 32px, a pane header is 22px and a row is 17px,
 * because the thing that limits how many phases you can see at once is chrome, not screen.
 */

import { RegistryProvider, useAtom, useAtomSet, useAtomValue } from '@effect/atom-react'
import * as Analysis from '@silklang/compiler/Analysis'
import type * as Target from '@silklang/compiler/Target'
import * as ToolchainPlan from '@silklang/compiler/ToolchainPlan'
import {
  type ViewContext,
  type ViewId,
  siblingsOf,
  viewById,
  views,
} from '@silklang/compiler/InspectorRegistry'
import type { DockviewApi, DockviewReadyEvent, IDockviewPanelProps } from 'dockview-react'
import { DockviewReact } from 'dockview-react'
import { Atom } from 'effect/unstable/reactivity'
import { useCallback, useEffect, useRef, useState } from 'react'
import { SilkEditor } from './editor'
import { PresetPalette } from './preset-palette'
import { type Preset, presetGroups, presets } from './presets'
import { EmptyState, RowList } from './row/row'
import {
  activeModuleAtom,
  analysisAtom,
  analysisInputAtom,
  countsAtom,
  cursorAtom,
  evaluationAtom,
  evaluationOptionsAtom,
  modeAtom,
  modulesAtom,
  profileAtom,
  programNameAtom,
  rootAtom,
  savedListAtom,
  savedWorkspacesAtom,
  snapshotAtom,
  targetAtom,
  trailAtom,
  workspacesAtom,
} from './state'
import {
  decodeLayout,
  decodeSource,
  encodeLayout,
  encodeSource,
  layoutParam,
  sourceParam,
} from './url-state'
import shell from './workbench.module.css'
import { seededWorkspaces, slotOrder, type Workspace } from './workspaces'

/** The phase picker: a 214px menu of all 16 views, opened from the pane's own title. */
function PhasePicker({
  active,
  onPick,
  onClose,
}: {
  readonly active: ViewId
  readonly onPick: (id: ViewId) => void
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
function SourceBody() {
  const [modules, setModules] = useAtom(modulesAtom)
  const activeModule = useAtomValue(activeModuleAtom)
  const snapshot = useAtomValue(snapshotAtom)
  const [cursor, setCursor] = useAtom(cursorAtom)
  const formatRef = useRef<(() => boolean) | null>(null)

  const names = Object.keys(modules)
  const active = names.includes(activeModule) ? activeModule : (names[0] ?? '')
  const text = modules[active] ?? ''

  // The editor speaks bare byte ranges; the shared cursor is module-qualified, so the active
  // module is attached here — the one place that knows which module the editor is showing.
  const onSelect = useCallback(
    (range: { readonly start: number; readonly end: number }) =>
      setCursor({ module: active, ...range }),
    [setCursor, active],
  )

  const activeRef = useRef(active)
  activeRef.current = active
  const onChange = useCallback(
    (value: string) => {
      setModules((current) => ({ ...current, [activeRef.current]: value }))
    },
    [setModules],
  )

  return (
    <>
      <SilkEditor
        value={text}
        snapshot={snapshot}
        module={active}
        cursor={cursor}
        onChange={onChange}
        onSelect={onSelect}
        formatRef={formatRef}
        className={shell.editor}
      />
      <div className={shell.sourceFooter}>
        <span>{active}</span>
        <span>{text.length} B</span>
        <span className={shell.spacer} />
        <button
          type="button"
          className={shell.footerAction}
          title="Format (⌘S or ⇧⌥F)"
          onClick={() => formatRef.current?.()}
        >
          format
        </button>
        <span>
          {cursor === undefined ? 'no selection' : `sel [${cursor.start}, ${cursor.end})`}
        </span>
      </div>
    </>
  )
}

/**
 * Every pane. One 22px bar carries the phase picker, sibling phases, any pane-local control and
 * the pane's meta; the body is either the editor or a list of rows.
 */
function ViewPane(props: IDockviewPanelProps<{ view: ViewId }>) {
  const [pickerOpen, setPickerOpen] = useState(false)
  const [filter, setFilter] = useState('')
  const [showTrivia, setShowTrivia] = useState(false)

  const snapshot = useAtomValue(snapshotAtom)
  const analysisInput = useAtomValue(analysisInputAtom)
  const mode = useAtomValue(modeAtom)
  const profile = useAtomValue(profileAtom)
  const cursor = useAtomValue(cursorAtom)
  const setCursor = useAtomSet(cursorAtom)
  const evaluation = useAtomValue(evaluationAtom)
  const evaluationOptions = useAtomValue(evaluationOptionsAtom)
  const setEvaluation = useAtomSet(evaluationAtom)

  const viewId = props.params.view
  const definition = viewById(viewId)

  const onPick = useCallback(
    (next: ViewId) => {
      const picked = viewById(next)
      if (picked === undefined) return
      props.api.updateParameters({ view: next })
      props.api.setTitle(picked.title)
    },
    [props.api],
  )

  if (definition === undefined) {
    // A pane with nothing to render is an invitation, not an error: name every view it could
    // become and let one click get there.
    return (
      <div className={shell.pane}>
        <div className={shell.paneBar}>
          <span className={shell.paneMeta}>empty pane</span>
        </div>
        <div className={shell.emptyPaneBody}>
          <div className={shell.emptyPaneChooser}>
            <p className={shell.emptyPaneHint}>
              {`“${viewId}” is not a view this workbench knows — pick one:`}
            </p>
            <div className={shell.emptyPaneList} role="menu">
              {views.map((view) => (
                <button
                  key={view.id}
                  type="button"
                  role="menuitem"
                  className={shell.pickerOption}
                  onClick={() => onPick(view.id)}
                >
                  <span className={shell.pickerTag}>{view.tag}</span>
                  <span className={shell.pickerTitle}>{view.title}</span>
                  <span className={shell.pickerPhase}>{view.phase}</span>
                </button>
              ))}
            </div>
          </div>
        </div>
      </div>
    )
  }

  const context: ViewContext = {
    snapshot,
    modules: analysisInput.modules,
    root: analysisInput.rootModule,
    mode,
    profile,
    evaluation,
    filter,
    showTrivia,
  }

  // The registry advertises actions as data; evaluation is the only one, and it runs the lowered
  // MIR, which is absent when the target did not resolve.
  const runAction = () =>
    setEvaluation(
      Analysis.mirOf(snapshot)._tag === 'Available'
        ? Analysis.evaluate(snapshot, evaluationOptions)
        : undefined,
    )

  const isSource = definition.id === 'source'
  const result = isSource ? undefined : definition.project(context)
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
          <button type="button" className={shell.paneToggle} onClick={runAction}>
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
        <SourceBody />
      ) : result?.unavailable !== undefined ? (
        <EmptyState reason>{result.unavailable}</EmptyState>
      ) : (
        <RowList
          rows={result?.rows ?? []}
          cursor={cursor}
          onPick={setCursor}
          label={`${definition.title} rows`}
        />
      )}

      {pickerOpen ? (
        <PhasePicker active={definition.id} onPick={onPick} onClose={() => setPickerOpen(false)} />
      ) : null}
    </div>
  )
}

const components = { view: ViewPane }

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
    title: viewById(a1)?.title ?? 'Source',
    params: { view: a1 },
  })
  const columnB = api.addPanel({
    id: 'pane-b1',
    component: 'view',
    title: viewById(b1)?.title ?? '',
    params: { view: b1 },
    position: { direction: 'right', referencePanel: columnA },
  })
  const columnC = api.addPanel({
    id: 'pane-c1',
    component: 'view',
    title: viewById(c1)?.title ?? '',
    params: { view: c1 },
    position: { direction: 'right', referencePanel: columnB },
  })

  api.addPanel({
    id: 'pane-a2',
    component: 'view',
    title: viewById(a2)?.title ?? '',
    params: { view: a2 },
    position: { direction: 'below', referencePanel: columnA },
  })
  api.addPanel({
    id: 'pane-b2',
    component: 'view',
    title: viewById(b2)?.title ?? '',
    params: { view: b2 },
    position: { direction: 'below', referencePanel: columnB },
  })
  api.addPanel({
    id: 'pane-c2',
    component: 'view',
    title: viewById(c2)?.title ?? '',
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

function WorkbenchInner() {
  const [modules, setModules] = useAtom(modulesAtom)
  const [root, setRoot] = useAtom(rootAtom)
  const [activeModule, setActiveModule] = useAtom(activeModuleAtom)
  const [programName, setProgramName] = useAtom(programNameAtom)
  const setEvaluationOptions = useAtomSet(evaluationOptionsAtom)
  const [profile, setProfile] = useAtom(profileAtom)
  const [target, setTarget] = useAtom(targetAtom)
  const cursor = useAtomValue(cursorAtom)
  const trail = useAtomValue(trailAtom)
  const counts = useAtomValue(countsAtom)
  const analysis = useAtomValue(analysisAtom)
  const workspaces = useAtomValue(workspacesAtom)
  const savedList = useAtomValue(savedListAtom)
  const setSavedWorkspaces = useAtomSet(savedWorkspacesAtom)

  const [paletteOpen, setPaletteOpen] = useState(false)
  const [sidebarOpen, setSidebarOpen] = useState(true)
  const [namingWorkspace, setNamingWorkspace] = useState(false)
  // Which project groups are expanded in the sidebar. Collapsed by default: 98 programs is a
  // list to opt into, not to scroll past.
  const [openGroups, setOpenGroups] = useState<ReadonlySet<string>>(new Set())
  const [api, setApi] = useState<DockviewApi>()
  const [activeWorkspace, setActiveWorkspace] = useState<string>('Backend triage')
  const paneCounter = useRef(0)
  const dockRef = useRef<HTMLDivElement>(null)

  // Multi-atom writes are batched so the snapshot never rebuilds against a root that is not in
  // the module map — a subscriber notified between the two writes would pull exactly that.
  const loadPreset = useCallback(
    (preset: Preset) => {
      Atom.batch(() => {
        setModules(preset.modules)
        setRoot(preset.root)
        setActiveModule(preset.root)
        setProgramName(preset.label)
        setEvaluationOptions(preset.evaluation ?? {})
      })
    },
    [setModules, setRoot, setActiveModule, setProgramName, setEvaluationOptions],
  )

  const addModule = (): void => {
    let index = modules.lib === undefined ? 0 : 2
    let name = index === 0 ? 'lib' : `lib${index}`
    while (modules[name] !== undefined) {
      index += 1
      name = `lib${index}`
    }
    Atom.batch(() => {
      setModules({ ...modules, [name]: 'pub fn answer() -> i32 { return 1 }' })
      setActiveModule(name)
    })
  }

  const addPane = (viewId: string): void => {
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
  }

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

  const onReady = useCallback(
    (event: DockviewReadyEvent) => {
      setApi(event.api)

      const params = new URLSearchParams(window.location.search)

      // A URL layout wins over the saved one: an explicit link is a request for that exact view.
      const restore = async (): Promise<void> => {
        const urlSource = params.get(sourceParam)
        if (urlSource !== null) {
          const decoded = await decodeSource(urlSource)
          if (decoded !== undefined) {
            Atom.batch(() => {
              setModules(decoded.modules)
              setRoot(decoded.root)
              setActiveModule(decoded.root)
              if (decoded.target !== undefined) setTarget(decoded.target as Target.Id)
            })
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
    },
    [setModules, setRoot, setActiveModule, setTarget],
  )

  // Keep the URL in step with the layout and the source, so a refresh or a shared link lands on
  // the same view. `replaceState` rather than push: dragging a pane should not fill up history.
  //
  // This stays hand-rolled rather than becoming Atom.searchParam: the payloads are
  // deflate-compressed and CompressionStream is async, while searchParam schemas must be
  // synchronous — and the layout's source of truth is dockview's event stream, not an atom.
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

  // The name comes from an inline input rather than `window.prompt`: embedded webviews block
  // prompt() outright, and a native dialog would be the only modal on a page that has none.
  const saveWorkspace = (name: string): void => {
    if (api === undefined || name.trim() === '') return
    // A workspace is the *arrangement*, so it stores the views its panes currently show.
    const panes = api.panels.slice(0, 6)
    const viewAt = (index: number): ViewId => {
      const params: unknown = panes[index]?.params
      if (typeof params !== 'object' || params === null || !('view' in params)) return 'source'
      const view = params.view
      return typeof view === 'string' ? (viewById(view)?.id ?? 'source') : 'source'
    }
    const entry: Workspace = {
      name: name.trim(),
      panes: {
        a1: viewAt(0),
        a2: viewAt(1),
        b1: viewAt(2),
        b2: viewAt(3),
        c1: viewAt(4),
        c2: viewAt(5),
      },
    }
    setSavedWorkspaces([...savedList.filter((candidate) => candidate.name !== entry.name), entry])
    setActiveWorkspace(entry.name)
  }

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
              // oxlint-disable-next-line jsx-a11y/no-autofocus -- appears on explicit click, in place of the trigger
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
                  <span className={shell.sidebarRowMeta}>{(modules[name] ?? '').length} B</span>
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

      <PresetPalette open={paletteOpen} onClose={() => setPaletteOpen(false)} onPick={loadPreset} />
    </div>
  )
}

/**
 * One RegistryProvider at the workbench root: a fresh registry per mount, and — because client
 * components still render once on the server — a fresh registry per SSR request, so no state
 * bleeds through the module-level default registry between requests.
 */
export function Workbench() {
  return (
    <RegistryProvider>
      <WorkbenchInner />
    </RegistryProvider>
  )
}
