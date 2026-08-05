'use client'

/**
 * The consolidated compiler workbench: one source program, every phase of the pipeline as a
 * panel you can toggle, collapse, and resize.
 *
 * The ten per-phase labs each owned their own editor and rebuilt their own snapshot, so seeing
 * one program through two phases meant retyping it on two pages. Here the snapshot is built once
 * and every panel reads from it, which is also what makes the phases comparable — a disagreement
 * between two panels is now about the compiler, not about two different inputs.
 */

import { Analysis, type ToolchainPlan } from '@silk-effect/compiler'
import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { IndexView } from './declaration-index/declaration-index'
import { DiscoveryView } from './instances/instances'
import { CfgView } from './mir-cfg/mir-cfg'
import { ClosureView } from './module-closure/module-closure'
import { OwnershipView } from './ownership/ownership'
import { LlvmView, ToolchainView, WasmView } from './panels'
import {
  DiagnosticPanel,
  diagnosticEntries,
  HirPanel,
  TokenStream,
} from './syntax-inspector/syntax-inspector'
import styles from './syntax-inspector/syntax-inspector.module.css'
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
  {
    label: 'Ambiguous call',
    source: `pub fn same() -> I32 { return 1 }
pub fn same() -> I32 { return 2 }
pub fn main() -> I32 { return same() }`,
  },
] as const

/** Panel ids are persisted in localStorage, so renaming one silently resets a saved layout. */
type PanelId =
  | 'syntax'
  | 'closure'
  | 'index'
  | 'hir'
  | 'ownership'
  | 'instances'
  | 'mir'
  | 'llvm'
  | 'wasm'
  | 'toolchain'

const defaultOpen: ReadonlyArray<PanelId> = ['syntax', 'mir', 'wasm']
const storageKey = 'silk-labs-workbench'

interface Layout {
  readonly open: ReadonlyArray<PanelId>
  readonly split: number
}

const isPanelId = (value: unknown): value is PanelId =>
  typeof value === 'string' && panelOrder.includes(value as PanelId)

const panelOrder: ReadonlyArray<PanelId> = [
  'syntax',
  'closure',
  'index',
  'hir',
  'ownership',
  'instances',
  'mir',
  'llvm',
  'wasm',
  'toolchain',
]

/** Reads a saved layout, ignoring anything malformed or referring to a since-renamed panel. */
const readLayout = (): Layout | undefined => {
  try {
    const raw = window.localStorage.getItem(storageKey)
    if (raw === null) return undefined
    const parsed: unknown = JSON.parse(raw)
    if (typeof parsed !== 'object' || parsed === null) return undefined
    const { open, split } = parsed as { open?: unknown; split?: unknown }
    return {
      open: Array.isArray(open) ? open.filter(isPanelId) : defaultOpen,
      split: typeof split === 'number' && split >= 15 && split <= 75 ? split : 38,
    }
  } catch {
    // A corrupt or unavailable store is not worth failing the page over.
    return undefined
  }
}

function Panel({
  id,
  phase,
  title,
  count,
  tone,
  open,
  onToggle,
  children,
}: {
  readonly id: PanelId
  readonly phase: string
  readonly title: string
  readonly count?: number | string
  readonly tone?: 'error'
  readonly open: boolean
  readonly onToggle: (id: PanelId) => void
  readonly children: React.ReactNode
}) {
  return (
    <section className={shell.panel} aria-labelledby={`panel-${id}`}>
      <button
        type="button"
        className={shell.panelBar}
        aria-expanded={open}
        // The visible title is inside this button, so it names the button implicitly; the
        // explicit label keeps the phase eyebrow and count badge out of that name.
        aria-label={`${title} — ${phase} phase`}
        onClick={() => onToggle(id)}
      >
        <span className={shell.chevron} aria-hidden="true">
          ▶
        </span>
        <span className={shell.panelPhase}>{phase}</span>
        <span id={`panel-${id}`}>{title}</span>
        <span className={shell.toolbarSpacer} />
        {count === undefined ? null : (
          <span className={shell.toggleCount} data-tone={tone}>
            {count}
          </span>
        )}
      </button>
      {open ? <div className={shell.panelBody}>{children}</div> : null}
    </section>
  )
}

export function Workbench() {
  const [text, setText] = useState<string>(presets[0].source)
  const [mode, setMode] = useState<'release' | 'debug'>('debug')
  const [profile, setProfile] = useState<ToolchainPlan.OptimizationProfile>('release')
  const [selectedDiagnostic, setSelectedDiagnostic] = useState<number>()
  const [open, setOpen] = useState<ReadonlyArray<PanelId>>(defaultOpen)
  const [split, setSplit] = useState(38)
  const splitRef = useRef<HTMLDivElement>(null)
  const [dragging, setDragging] = useState(false)

  // Layout is restored after mount rather than during render: the server has no localStorage,
  // and reading it inline would make the first client render disagree with the server's.
  useEffect(() => {
    const saved = readLayout()
    if (saved === undefined) return
    setOpen(saved.open)
    setSplit(saved.split)
  }, [])

  useEffect(() => {
    try {
      window.localStorage.setItem(storageKey, JSON.stringify({ open, split }))
    } catch {
      // Private-mode quota failures should not break the lab.
    }
  }, [open, split])

  const toggle = useCallback((id: PanelId) => {
    setOpen((current) =>
      current.includes(id) ? current.filter((entry) => entry !== id) : [...current, id],
    )
  }, [])

  // Pointer capture keeps the drag alive over the panels and past the window edge.
  //
  // The drag flag is a ref, not just state: `pointermove` can fire before React has re-rendered
  // from the `pointerdown`, and a state-only flag leaves the move handler reading a stale `false`
  // and dropping the drag entirely. The state copy exists only to style the handle.
  const draggingRef = useRef(false)

  const onHandleDown = useCallback((event: React.PointerEvent<HTMLButtonElement>) => {
    event.currentTarget.setPointerCapture(event.pointerId)
    draggingRef.current = true
    setDragging(true)
  }, [])

  const onHandleMove = useCallback((event: React.PointerEvent<HTMLButtonElement>) => {
    if (!draggingRef.current) return
    const bounds = splitRef.current?.getBoundingClientRect()
    if (bounds === undefined) return
    const next = ((event.clientX - bounds.left) / bounds.width) * 100
    setSplit(Math.min(75, Math.max(15, next)))
  }, [])

  const onHandleUp = useCallback(() => {
    draggingRef.current = false
    setDragging(false)
  }, [])

  const snapshot = useMemo(() => Analysis.ofSource(sourceId, encoder.encode(text)), [text])
  const analysis = useMemo(() => Analysis.rootAnalysis(snapshot), [snapshot])
  const syntax = analysis.syntax
  const diagnostics = useMemo(() => Analysis.diagnostics(snapshot), [snapshot])
  const closure = snapshot.closure
  const index = useMemo(() => Analysis.declarationIndex(snapshot), [snapshot])
  const discovery = useMemo(() => Analysis.instancesOf(snapshot), [snapshot])
  const ownership = useMemo(
    () => Analysis.ownershipOf(snapshot, closure.rootModule),
    [snapshot, closure],
  )
  const mir = useMemo(() => Analysis.loweredMir(snapshot), [snapshot])

  const isOpen = (id: PanelId): boolean => open.includes(id)

  return (
    <div className={shell.workbench}>
      <div className={shell.toolbar}>
        <h1 className={shell.toolbarTitle}>Compiler workbench</h1>
        {panelOrder.map((id) => (
          <button
            key={id}
            type="button"
            className={shell.toggle}
            aria-pressed={isOpen(id)}
            onClick={() => toggle(id)}
          >
            {id}
          </button>
        ))}
        <span className={shell.toolbarSpacer} />
        <button
          type="button"
          className={shell.toggle}
          onClick={() => setMode(mode === 'release' ? 'debug' : 'release')}
          aria-pressed={mode === 'debug'}
        >
          {mode}
        </button>
      </div>

      <div className={shell.split} ref={splitRef}>
        <div className={shell.sourceColumn} style={{ width: `${split}%` }}>
          <div className={styles.exampleBar} aria-label="Source presets">
            {presets.map((preset) => (
              <button
                key={preset.label}
                type="button"
                onClick={() => {
                  setText(preset.source)
                  setSelectedDiagnostic(undefined)
                }}
              >
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
            value={text}
            onChange={(event) => {
              setText(event.target.value)
              setSelectedDiagnostic(undefined)
            }}
            spellCheck={false}
            autoCapitalize="off"
            autoCorrect="off"
          />

          <div className={shell.sourceScroll}>
            <dl className={styles.metrics}>
              <div>
                <dt>UTF-8 bytes</dt>
                <dd>{syntax.source.bytes.length}</dd>
              </div>
              <div>
                <dt>Tokens</dt>
                <dd>{syntax.tokens.length}</dd>
              </div>
              <div>
                <dt>Diagnostics</dt>
                <dd>{diagnostics.length}</dd>
              </div>
            </dl>

            <div className={styles.diagnostics}>
              <DiagnosticPanel
                entries={diagnosticEntries(
                  syntax.lexicalDiagnostics,
                  syntax.parserDiagnostics,
                  analysis.diagnostics,
                )}
                selected={selectedDiagnostic}
                onSelect={setSelectedDiagnostic}
              />
            </div>
          </div>
        </div>

        <button
          type="button"
          className={shell.handle}
          data-dragging={dragging}
          aria-label="Resize source panel"
          onPointerDown={onHandleDown}
          onPointerMove={onHandleMove}
          onPointerUp={onHandleUp}
          onPointerCancel={onHandleUp}
          onKeyDown={(event) => {
            if (event.key === 'ArrowLeft') setSplit((v) => Math.max(15, v - 2))
            if (event.key === 'ArrowRight') setSplit((v) => Math.min(75, v + 2))
          }}
        />

        <div className={shell.panelColumn}>
          {open.length === 0 ? (
            <p className={shell.empty}>
              Every panel is hidden. Use the toggles above to bring a phase back.
            </p>
          ) : null}

          {isOpen('syntax') ? (
            <Panel
              id="syntax"
              phase="syntax"
              title="Tokens"
              count={syntax.tokens.length}
              open
              onToggle={toggle}
            >
              <TokenStream syntax={syntax} />
            </Panel>
          ) : null}

          {isOpen('closure') ? (
            <Panel
              id="closure"
              phase="modules"
              title="Module closure"
              count={Analysis.modules(snapshot).length}
              open
              onToggle={toggle}
            >
              <ClosureView closure={closure} />
            </Panel>
          ) : null}

          {isOpen('index') ? (
            <Panel
              id="index"
              phase="headers"
              title="Declaration index"
              count={index.modules.reduce((sum, module) => sum + module.declarations.length, 0)}
              open
              onToggle={toggle}
            >
              <IndexView index={index} />
            </Panel>
          ) : null}

          {isOpen('hir') ? (
            <Panel
              id="hir"
              phase="elaboration"
              title="Typed HIR"
              count={analysis.hir.functions.length}
              open
              onToggle={toggle}
            >
              <HirPanel hir={analysis.hir} />
            </Panel>
          ) : null}

          {isOpen('ownership') ? (
            <Panel
              id="ownership"
              phase="ownership"
              title="Ownership + cleanup"
              count={ownership?.functions.length ?? 0}
              open
              onToggle={toggle}
            >
              {ownership === undefined ? (
                <p className={styles.emptyState}>Ownership unavailable for this program</p>
              ) : (
                <OwnershipView facts={ownership} />
              )}
            </Panel>
          ) : null}

          {isOpen('instances') ? (
            <Panel
              id="instances"
              phase="discovery"
              title="Instances"
              count={
                discovery.entry._tag === 'Resolved' ? discovery.instances.length : 'entry unavailable'
              }
              tone={discovery.entry._tag === 'Resolved' ? undefined : 'error'}
              open
              onToggle={toggle}
            >
              <DiscoveryView discovery={discovery} />
            </Panel>
          ) : null}

          {isOpen('mir') ? (
            <Panel
              id="mir"
              phase="lowering"
              title="MIR control flow"
              count={mir.functions.length}
              open
              onToggle={toggle}
            >
              <CfgView module={mir} source={text} />
            </Panel>
          ) : null}

          {isOpen('llvm') ? (
            <Panel id="llvm" phase="backend" title="LLVM IR" open onToggle={toggle}>
              <LlvmView snapshot={snapshot} mode={mode} />
            </Panel>
          ) : null}

          {isOpen('wasm') ? (
            <Panel id="wasm" phase="backend" title="WebAssembly" open onToggle={toggle}>
              <WasmView snapshot={snapshot} mode={mode} />
            </Panel>
          ) : null}

          {isOpen('toolchain') ? (
            <Panel
              id="toolchain"
              phase="native"
              title="Toolchain (planned)"
              count={profile}
              open
              onToggle={toggle}
            >
              <div className={styles.exampleBar} aria-label="Optimization profiles">
                {(['debug', 'release', 'release-with-debug'] as const).map((candidate) => (
                  <button
                    key={candidate}
                    type="button"
                    onClick={() => setProfile(candidate)}
                    aria-pressed={profile === candidate}
                  >
                    {candidate}
                  </button>
                ))}
              </div>
              <ToolchainView snapshot={snapshot} profile={profile} />
            </Panel>
          ) : null}
        </div>
      </div>
    </div>
  )
}
