/**
 * Workbench state as atoms.
 *
 * The panes dockview renders are portaled out of the tree that owns workbench state, so state
 * used to reach them through a hand-rolled ref-plus-listeners store that re-rendered every pane
 * on every render. Atoms are addressable from anywhere under the one RegistryProvider, so each
 * pane now subscribes to exactly the state it reads and the store is gone.
 *
 * Derived values (snapshot, analysis, counts, trail) are atoms too: computed once per registry,
 * cached until a dependency changes, instead of per-component `useMemo` chains.
 */

import { Analysis, MirVerification, ToolchainPlan } from '@silk-lang/compiler'
import type { BootstrapEvaluation, Target } from '@silk-lang/compiler'
import { diagnosticCounts, diagnosticEntries, hirContract } from '@silk-lang/inspector/ProjectSyntax'
import type { Span } from '@silk-lang/inspector/Row'
import { KeyValueStore } from 'effect/unstable/persistence'
import { AsyncResult, Atom } from 'effect/unstable/reactivity'
import { presets } from './presets'
import * as Snapshot from './snapshot'
import {
  seededWorkspaces,
  type Workspace,
  workspacesSchema,
  workspaceStorageKey,
} from './workspaces'

export const initialPreset = presets.find((preset) => preset.label === 'Nested calls') ?? presets[0]

export const modulesAtom = Atom.make<Readonly<Record<string, string>>>(initialPreset.modules)
export const rootAtom = Atom.make(initialPreset.root)
export const activeModuleAtom = Atom.make(initialPreset.root)
export const programNameAtom = Atom.make(initialPreset.label)
export const profileAtom = Atom.make<ToolchainPlan.OptimizationProfile>('release')
export const targetAtom = Atom.make<Target.Id>('aarch64-apple-darwin')
export const evaluationOptionsAtom = Atom.make<BootstrapEvaluation.Options>(
  initialPreset.evaluation ?? {},
)

/** Idle time before one editing burst becomes a new compiler input. */
export const sourceUpdateDebounceMs = 250

/**
 * Codegen's debug-info mode is derived from the profile rather than being its own control: the
 * profile already says whether debug info is wanted (`-g`), so a separate toggle only adds states
 * where the two disagree.
 */
export const modeAtom = Atom.map(profileAtom, ToolchainPlan.codegenModeFor)

const encoder = new TextEncoder()

/** Root and sources settle together, so a preset/module switch can never expose a mismatched pair. */
const snapshotInputAtom = Atom.make((get) => ({
  rootModule: get(rootAtom),
  modules: get(modulesAtom),
}))

/** Root and sources currently represented by analysis, behind one shared debounce timer. */
export const analysisInputAtom = snapshotInputAtom.pipe(Atom.debounce(sourceUpdateDebounceMs))

/** One snapshot per settled editing burst, shared by every pane. */
export const snapshotAtom = Atom.make((get) => {
  const input = get(analysisInputAtom)
  return Snapshot.make({
    rootModule: input.rootModule,
    sources: new Map(
      Object.entries(input.modules).map(([name, text]) => [name, encoder.encode(text)]),
    ),
    target: get(targetAtom),
  })
})

export const analysisAtom = Atom.map(snapshotAtom, Analysis.rootAnalysis)

export const countsAtom = Atom.make((get) => {
  const analysis = get(analysisAtom)
  return diagnosticCounts(
    diagnosticEntries(
      analysis.syntax.lexicalDiagnostics,
      analysis.syntax.parserDiagnostics,
      analysis.diagnostics,
    ),
  )
})

/**
 * The workbench-level span cursor: one byte range, shared by every pane.
 *
 * The read depends on live modules as well as the settled snapshot, so a source edit clears the
 * selection immediately instead of keeping a stale span visible during the debounce window.
 */
export const cursorAtom = Atom.writable<Span | undefined, Span | undefined>(
  (get) => {
    get(modulesAtom)
    get(snapshotAtom)
    return undefined
  },
  (ctx, span) => ctx.setSelf(span),
)

/**
 * An evaluation describes the program that produced it, so it cannot outlive that program. It
 * uses the same immediate live-source reset as the cursor; editing never implies a new run.
 */
export const evaluationAtom = Atom.writable<
  BootstrapEvaluation.Outcome | undefined,
  BootstrapEvaluation.Outcome | undefined
>(
  (get) => {
    get(modulesAtom)
    get(snapshotAtom)
    return undefined
  },
  (ctx, outcome) => ctx.setSelf(outcome),
)

export interface TrailCell {
  readonly phase: string
  readonly value: string
  readonly missing?: boolean
}

/** The span cursor read through every phase — the join that links the panes. */
export const trailAtom = Atom.make((get): ReadonlyArray<TrailCell> => {
  const cursor = get(cursorAtom)
  if (cursor === undefined) return []
  const input = get(analysisInputAtom)
  const source = input.modules[cursor.module] ?? ''
  const analysis = get(analysisAtom)
  const snapshot = get(snapshotAtom)

  const slice = source.slice(cursor.start, cursor.end)
  const cells: Array<TrailCell> = [
    { phase: 'src', value: slice.trim() === '' ? '—' : slice.trim() },
  ]

  // The syntax and HIR cells read the root module's analysis, so a cursor in another module
  // has no construct there — the same offsets in a different file are a different construct.
  const inRoot = cursor.module === input.rootModule
  const token = inRoot
    ? analysis.syntax.tokens.find(
        (candidate) => candidate.span.start <= cursor.start && candidate.span.end >= cursor.end,
      )
    : undefined
  cells.push({ phase: 'cst', value: token?.kind ?? 'no token', missing: token === undefined })

  const fn = inRoot
    ? analysis.hir.functions.find(
        (candidate) =>
          candidate.declaration.syntax.span.start <= cursor.start &&
          candidate.declaration.syntax.span.end >= cursor.end,
      )
    : undefined
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
            MirVerification.operations(candidate).some(
              (operation) =>
                operation.provenance.span.sourceId === cursor.module &&
                operation.provenance.span.start <= cursor.start &&
                operation.provenance.span.end >= cursor.end,
            ),
          )?.id.name ?? 'no operation')
        : 'unavailable',
    missing: mir._tag !== 'Available',
  })

  return cells
})

const kvsRuntime = Atom.runtime(KeyValueStore.layerStorage(() => window.localStorage))

/**
 * Saved workspaces use the current exact schema. Invalid or retired data decodes as failure and
 * the seeded arrangements remain available.
 *
 * Async mode plus a server-initial value keeps hydration honest: the server and the first client
 * render both see no saved workspaces, and the decoded ones arrive as a post-hydration update —
 * a corrupt value reads as "no saved workspaces" rather than taking the page down.
 */
export const savedWorkspacesAtom = Atom.kvs({
  runtime: kvsRuntime,
  key: workspaceStorageKey,
  schema: workspacesSchema,
  defaultValue: () => [],
  mode: 'async',
}).pipe(Atom.withServerValueInitial)

const noSavedWorkspaces: ReadonlyArray<Workspace> = Object.freeze([])

export const savedListAtom = Atom.make((get): ReadonlyArray<Workspace> =>
  AsyncResult.getOrElse(get(savedWorkspacesAtom), () => noSavedWorkspaces),
)

/** Seeded arrangements plus saved ones; a saved workspace shadows a seeded one of the same name. */
export const workspacesAtom = Atom.make((get): ReadonlyArray<Workspace> => {
  const saved = get(savedListAtom)
  return [...seededWorkspaces.filter((seeded) => !saved.some((w) => w.name === seeded.name)), ...saved]
})
