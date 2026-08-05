'use client'

/**
 * What a pane can show.
 *
 * Panes are keyed by an instance id rather than by phase, so the same phase can be open more than
 * once — two MIR panes side by side is a legitimate thing to want when comparing presets. The
 * phase a pane renders is therefore pane *parameters*, not pane identity.
 */

import { Analysis } from '@silk-effect/compiler'
import type { ToolchainPlan } from '@silk-effect/compiler'
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

export type ViewId =
  | 'source'
  | 'diagnostics'
  | 'tokens'
  | 'closure'
  | 'index'
  | 'hir'
  | 'ownership'
  | 'instances'
  | 'mir'
  | 'llvm'
  | 'wasm'
  | 'toolchain'

/** Everything a view needs to render. Built once per source edit and shared by every pane. */
export interface ViewContext {
  readonly snapshot: Analysis.Snapshot
  readonly source: string
  readonly mode: 'release' | 'debug'
  readonly profile: ToolchainPlan.OptimizationProfile
  /**
   * Diagnostic selection is workbench-level rather than pane-level: selecting a diagnostic is a
   * statement about the program, so two open diagnostic panes should agree on it.
   */
  readonly selectedDiagnostic: number | undefined
  readonly onSelectDiagnostic: (index: number | undefined) => void
}

export interface ViewDefinition {
  readonly id: ViewId
  readonly title: string
  readonly phase: string
  readonly render: (context: ViewContext) => React.ReactNode
}

/** Ordered for the "add pane" menu: roughly the order the compiler runs its phases. */
export const views: ReadonlyArray<ViewDefinition> = [
  {
    id: 'tokens',
    title: 'Tokens',
    phase: 'syntax',
    render: ({ snapshot }) => <TokenStream syntax={Analysis.rootAnalysis(snapshot).syntax} />,
  },
  {
    id: 'closure',
    title: 'Module closure',
    phase: 'modules',
    render: ({ snapshot }) => <ClosureView closure={snapshot.closure} />,
  },
  {
    id: 'index',
    title: 'Declaration index',
    phase: 'headers',
    render: ({ snapshot }) => <IndexView index={Analysis.declarationIndex(snapshot)} />,
  },
  {
    id: 'hir',
    title: 'Typed HIR',
    phase: 'elaboration',
    render: ({ snapshot }) => <HirPanel hir={Analysis.rootAnalysis(snapshot).hir} />,
  },
  {
    id: 'ownership',
    title: 'Ownership + cleanup',
    phase: 'ownership',
    render: ({ snapshot }) => {
      const facts = Analysis.ownershipOf(snapshot, snapshot.closure.rootModule)
      return facts === undefined ? (
        <p className={styles.emptyState}>Ownership unavailable for this program</p>
      ) : (
        <OwnershipView facts={facts} />
      )
    },
  },
  {
    id: 'instances',
    title: 'Instances',
    phase: 'discovery',
    render: ({ snapshot }) => <DiscoveryView discovery={Analysis.instancesOf(snapshot)} />,
  },
  {
    id: 'mir',
    title: 'MIR control flow',
    phase: 'lowering',
    render: ({ snapshot, source }) => (
      <CfgView module={Analysis.loweredMir(snapshot)} source={source} />
    ),
  },
  {
    id: 'llvm',
    title: 'LLVM IR',
    phase: 'backend',
    render: ({ snapshot, mode }) => <LlvmView snapshot={snapshot} mode={mode} />,
  },
  {
    id: 'wasm',
    title: 'WebAssembly',
    phase: 'backend',
    render: ({ snapshot, mode }) => <WasmView snapshot={snapshot} mode={mode} />,
  },
  {
    id: 'toolchain',
    title: 'Toolchain (planned)',
    phase: 'native',
    render: ({ snapshot, profile }) => <ToolchainView snapshot={snapshot} profile={profile} />,
  },
  {
    id: 'diagnostics',
    title: 'Diagnostics',
    phase: 'all phases',
    render: ({ snapshot, selectedDiagnostic, onSelectDiagnostic }) => {
      const analysis = Analysis.rootAnalysis(snapshot)
      return (
        <div className={styles.diagnostics}>
          <DiagnosticPanel
            entries={diagnosticEntries(
              analysis.syntax.lexicalDiagnostics,
              analysis.syntax.parserDiagnostics,
              analysis.diagnostics,
            )}
            selected={selectedDiagnostic}
            onSelect={onSelectDiagnostic}
          />
        </div>
      )
    },
  },
]

const byId = new Map(views.map((view) => [view.id, view]))

export const viewById = (id: string): ViewDefinition | undefined => byId.get(id as ViewId)
