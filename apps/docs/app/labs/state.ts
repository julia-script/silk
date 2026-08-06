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

import { Analysis, ToolchainPlan } from '@silk-effect/compiler'
import type { BootstrapEvaluation, Target } from '@silk-effect/compiler'
import * as Schema from 'effect/Schema'
import { KeyValueStore } from 'effect/unstable/persistence'
import { AsyncResult, Atom } from 'effect/unstable/reactivity'
import { presets } from './presets'
import { diagnosticCounts, diagnosticEntries, hirContract } from './row/project-syntax'
import type { Span } from './row/row'
import * as Snapshot from './snapshot'
import { seededWorkspaces, type Workspace, workspaceStorageKey } from './workspaces'

export const initialPreset = presets.find((preset) => preset.label === 'Nested calls') ?? presets[0]

export const modulesAtom = Atom.make<Readonly<Record<string, string>>>(initialPreset.modules)
export const rootAtom = Atom.make(initialPreset.root)
export const activeModuleAtom = Atom.make(initialPreset.root)
export const programNameAtom = Atom.make(initialPreset.label)
export const profileAtom = Atom.make<ToolchainPlan.OptimizationProfile>('release')
export const targetAtom = Atom.make<Target.Id>('aarch64-apple-darwin')

/**
 * Codegen's debug-info mode is derived from the profile rather than being its own control: the
 * profile already says whether debug info is wanted (`-g`), so a separate toggle only adds states
 * where the two disagree.
 */
export const modeAtom = Atom.map(profileAtom, ToolchainPlan.codegenModeFor)

const encoder = new TextEncoder()

/** One snapshot per source edit, shared by every pane — what makes phases comparable. */
export const snapshotAtom = Atom.make((get) =>
  Snapshot.make({
    rootModule: get(rootAtom),
    sources: new Map(
      Object.entries(get(modulesAtom)).map(([name, text]) => [name, encoder.encode(text)]),
    ),
    target: get(targetAtom),
  }),
)

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
 * The read depends on the snapshot, so a source edit rebuilds the node and clears the selection
 * by construction — a cursor cannot outlive the program it points into. This replaces the manual
 * "reset on snapshot change" effect, which had to be remembered.
 */
export const cursorAtom = Atom.writable<Span | undefined, Span | undefined>(
  (get) => {
    get(snapshotAtom)
    return undefined
  },
  (ctx, span) => ctx.setSelf(span),
)

/**
 * An evaluation describes the program that produced it, so it cannot outlive that program: same
 * snapshot-keyed reset as the cursor. Evaluation is an explicit action, never implied by editing.
 */
export const evaluationAtom = Atom.writable<
  BootstrapEvaluation.Outcome | undefined,
  BootstrapEvaluation.Outcome | undefined
>(
  (get) => {
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
  const source = get(modulesAtom)[get(rootAtom)] ?? ''
  const analysis = get(analysisAtom)
  const snapshot = get(snapshotAtom)

  const slice = source.slice(cursor.start, cursor.end)
  const cells: Array<TrailCell> = [
    { phase: 'src', value: slice.trim() === '' ? '—' : slice.trim() },
  ]

  const token = analysis.syntax.tokens.find(
    (candidate) => candidate.span.start <= cursor.start && candidate.span.end >= cursor.end,
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
})

const kvsRuntime = Atom.runtime(KeyValueStore.layerStorage(() => window.localStorage))

/**
 * Loose on purpose: a saved workspace is user data that survives deploys, so pane ids from an
 * older build must decode (viewById resolves renamed ids) rather than failing the whole array.
 */
const savedWorkspacesSchema = Schema.Array(
  Schema.Struct({ name: Schema.String, panes: Schema.Record(Schema.String, Schema.String) }),
)

/**
 * Saved workspaces, persisted under the same key and JSON shape the pre-atom workbench wrote.
 *
 * Async mode plus a server-initial value keeps hydration honest: the server and the first client
 * render both see no saved workspaces, and the decoded ones arrive as a post-hydration update —
 * the same sequence the old read-in-an-effect produced. A corrupt or legacy-shaped value decodes
 * to a failure, which reads as "no saved workspaces" rather than taking the page down.
 */
export const savedWorkspacesAtom = Atom.kvs({
  runtime: kvsRuntime,
  key: workspaceStorageKey,
  schema: savedWorkspacesSchema,
  defaultValue: () => [],
  mode: 'async',
}).pipe(Atom.withServerValueInitial)

export const savedListAtom = Atom.make(
  (get): ReadonlyArray<Workspace> =>
    // Same unchecked narrowing the old defensive JSON read performed: pane ids are resolved (and
    // renamed ones forwarded) by viewById at use time, not validated here.
    AsyncResult.getOrElse(get(savedWorkspacesAtom), () => []) as unknown as ReadonlyArray<Workspace>,
)

/** Seeded arrangements plus saved ones; a saved workspace shadows a seeded one of the same name. */
export const workspacesAtom = Atom.make((get): ReadonlyArray<Workspace> => {
  const saved = get(savedListAtom)
  return [...seededWorkspaces.filter((seeded) => !saved.some((w) => w.name === seeded.name)), ...saved]
})
