/**
 * What a pane can show.
 *
 * Panes are keyed by an instance id rather than by phase, so the same phase can be open more than
 * once — two MIR panes side by side is a legitimate thing to want when comparing presets. The
 * phase a pane renders is therefore pane *parameters*, not pane identity.
 *
 * A view now returns rows plus a little header material rather than arbitrary markup. That is
 * what makes the 22px pane header possible: the header needs the pane's meta (`39/48`, `2 fn`,
 * `1 err`) and optional one-line fact strip, and only the view knows them.
 */

import type { BootstrapEvaluation, ToolchainPlan as ToolchainPlanNs } from '@silk-effect/compiler'
import { Analysis, ToolchainPlan } from '@silk-effect/compiler'
import { projectDataFlow } from './FlowModel.js'
import { backendEmission, execute, toolchainCommands } from './Panels.js'
import {
  arrayValueRows,
  backendControlRows,
  backendTextRows,
  closureRows,
  evaluationRows,
  hexRows,
  indexRows,
  instanceRows,
  layoutRows,
  mirRows,
  ownershipRows,
  pipelineRows,
  resolutionRows,
  structValueRows,
  symbolRows,
  toolchainRows,
} from './ProjectBackend.js'
import {
  diagnosticCounts,
  diagnosticEntries,
  diagnosticRows,
  flowRows,
  hirRows,
  tokenRows,
  treeRows,
} from './ProjectSyntax.js'
import type { RowModel } from './Row.js'

export type ViewId =
  | 'source'
  | 'diagnostics'
  | 'tokens'
  | 'tree'
  | 'flow'
  | 'evaluation'
  | 'closure'
  | 'index'
  | 'resolution'
  | 'hir'
  | 'struct-values'
  | 'array-values'
  | 'ownership'
  | 'instances'
  | 'layout'
  | 'mir'
  // One backend view, not one per backend: the target picks the backend.
  | 'backend'
  | 'toolchain'
  | 'pipeline'

/** One cell of a pane's optional 19px fact strip. */
export interface Fact {
  readonly text: string
  readonly tone?: 'symbol' | 'ok' | 'warning' | 'muted' | undefined
}

/** Everything a view needs to render. Built once per source edit and shared by every pane. */
export interface ViewContext {
  readonly snapshot: Analysis.Snapshot
  /** Every module represented by `snapshot`, keyed by module name. */
  readonly modules: Readonly<Record<string, string>>
  /** Which module the request is rooted at — the entry the driver starts from. */
  readonly root: string
  readonly mode: 'release' | 'debug'
  readonly profile: ToolchainPlanNs.OptimizationProfile
  /**
   * Evaluation is an explicit action, never implied by editing: the green semantic layer states
   * what the program means, and only an actual run may claim what it does. The consumer runs it
   * (when its surface's run action fires) and hands the outcome in here.
   */
  readonly evaluation: BootstrapEvaluation.Outcome | undefined
  /** Pane-local view state, keyed by pane id — the CST filter and trivia toggle live here. */
  readonly filter: string
  readonly showTrivia: boolean
}

/** What a view hands back to its pane. */
export interface ViewResult {
  readonly rows: ReadonlyArray<RowModel>
  /** Right-aligned in the pane header: `39/48`, `2 fn`, `1 err`. */
  readonly meta?: string | undefined
  readonly facts?: ReadonlyArray<Fact> | undefined
  /** Set when the phase produced nothing, and says which phase broke and why. */
  readonly unavailable?: string | undefined
}

export interface ViewDefinition {
  readonly id: ViewId
  readonly title: string
  readonly phase: string
  /** Three-letter code for the picker menu and the span bar. */
  readonly tag: string
  /** Panes in the same group offer each other as one-click sibling tabs. */
  readonly group: string
  readonly hasFilter?: boolean
  /**
   * Views that need an explicit action advertise it here; the consumer renders the control and
   * performs the action itself (rows are data, so the registry cannot carry a callback).
   *
   * Evaluation is the only one: the semantic phases state what the program *means* and are safe
   * to recompute on every keystroke, but only an actual run may claim what it *does*.
   */
  readonly action?: { readonly label: string }
  readonly project: (context: ViewContext) => ViewResult
}

const noRows: ReadonlyArray<RowModel> = []

/** Ordered roughly the way the compiler runs its phases. */
export const views: ReadonlyArray<ViewDefinition> = [
  {
    id: 'source',
    title: 'Source',
    phase: 'input',
    tag: 'SRC',
    group: 'input',
    // The editor is the one bespoke body; the pane renders a textarea instead of rows.
    project: () => ({ rows: noRows }),
  },
  {
    id: 'tokens',
    title: 'Tokens',
    phase: 'syntax',
    tag: 'TOK',
    group: 'syntax',
    hasFilter: true,
    project: ({ snapshot, showTrivia, filter }) => {
      const syntax = Analysis.rootAnalysis(snapshot).syntax
      const rows = tokenRows(syntax, showTrivia, filter)
      return { rows, meta: `${rows.length}/${syntax.tokens.length}` }
    },
  },
  {
    id: 'tree',
    title: 'Concrete tree',
    phase: 'syntax',
    tag: 'CST',
    group: 'syntax',
    hasFilter: true,
    project: ({ snapshot, showTrivia, filter }) => {
      const syntax = Analysis.rootAnalysis(snapshot).syntax
      const rows = treeRows(syntax, showTrivia, filter)
      const missing = rows.filter((row) => row.dot === 'missing').length
      return {
        rows,
        meta: missing === 0 ? `${rows.length}` : `${rows.length} · ${missing} missing`,
      }
    },
  },
  {
    id: 'closure',
    title: 'Module closure',
    phase: 'modules',
    tag: 'MOD',
    group: 'modules',
    project: ({ snapshot }) => {
      const closure = snapshot.closure
      return {
        rows: closureRows(closure),
        meta: `${closure.modules.length} mod`,
      }
    },
  },
  {
    id: 'index',
    title: 'Declaration index',
    phase: 'headers',
    tag: 'IDX',
    group: 'modules',
    project: ({ snapshot }) => {
      const index = Analysis.declarationIndex(snapshot)
      const declarations = index.modules.reduce((total, module) => total + module.members.length, 0)
      return {
        rows: indexRows(index),
        meta: `${declarations} decl`,
      }
    },
  },
  {
    id: 'resolution',
    title: 'Name resolution',
    phase: 'names',
    tag: 'NAM',
    group: 'names',
    project: ({ snapshot }) => {
      const resolution = Analysis.nameResolution(snapshot)
      const bindings = resolution.modules.reduce((total, scope) => total + scope.bindings.length, 0)
      return {
        rows: resolutionRows(resolution),
        meta: `${bindings} bind`,
      }
    },
  },
  {
    id: 'hir',
    title: 'Typed HIR',
    phase: 'elaboration',
    tag: 'HIR',
    group: 'semantics',
    project: ({ snapshot }) => {
      const hir = Analysis.rootAnalysis(snapshot).hir
      const conversions = Analysis.hirUnionConversionsOf(snapshot, snapshot.closure.rootModule)
      return {
        rows: hirRows(hir),
        facts:
          conversions.length === 0
            ? undefined
            : [{ text: `${conversions.length} union conversion`, tone: 'symbol' }],
        meta: `${hir.functions.length} fn${conversions.length === 0 ? '' : ` · ${conversions.length} union`}`,
      }
    },
  },
  {
    id: 'flow',
    title: 'Data flow',
    phase: 'semantics',
    tag: 'FLO',
    group: 'semantics',
    /**
     * Value flow is the one view about *relationships* rather than about a tree: which argument
     * binds to which parameter, and what a run actually computed for each. The evaluated overlay
     * is layered on when a run has happened, so the same rows read as semantics-only until then.
     */
    project: ({ snapshot, evaluation }) => {
      const flow = projectDataFlow(Analysis.rootAnalysis(snapshot), evaluation)
      if (flow.status === 'Empty') {
        return { rows: noRows, unavailable: `No value path — ${flow.summary}` }
      }
      return {
        rows: flowRows(flow),
        meta: `${flow.status.toLowerCase()} · ${flow.mode.toLowerCase()}`,
      }
    },
  },
  {
    id: 'struct-values',
    title: 'Struct values',
    phase: 'elaboration + ABI',
    tag: 'STR',
    group: 'semantics',
    /**
     * One aggregate story in one pane: literal field mappings (source order vs canonical order),
     * the projection chain, the compiler-owned scalar calling shapes, and — after a run — the
     * Construct/Project/Cleanup events that realized them.
     */
    project: ({ snapshot, root, evaluation }) => {
      const literals = Analysis.structLiteralsOf(snapshot, root)
      const projections = Analysis.fieldProjectionsOf(snapshot, root)
      const layout = Analysis.layoutOf(snapshot)
      const shapes = layout._tag === 'Available' ? layout.value.callingShapes : []
      return {
        rows: structValueRows(literals, projections, shapes, evaluation),
        meta: `${literals.length} lit · ${projections.length} proj`,
      }
    },
  },
  {
    id: 'array-values',
    title: 'Array values',
    phase: 'elaboration + layout + evaluation',
    tag: 'ARR',
    group: 'semantics',
    project: ({ snapshot, root, evaluation }) => {
      const types = Analysis.fixedArrayTypesOf(snapshot, root)
      const literals = Analysis.arrayLiteralsOf(snapshot, root)
      const projections = Analysis.indexProjectionsOf(snapshot, root)
      return {
        rows: arrayValueRows(
          types,
          literals,
          projections,
          Analysis.repeatedLayoutsOf(snapshot),
          Analysis.arrayCallingShapesOf(snapshot),
          evaluation,
        ),
        meta: `${literals.length} lit · ${projections.length} index`,
      }
    },
  },
  {
    id: 'ownership',
    title: 'Ownership + cleanup',
    phase: 'ownership',
    tag: 'OWN',
    group: 'semantics',
    project: ({ snapshot }) => {
      const facts = Analysis.ownershipOf(snapshot, snapshot.closure.rootModule)
      if (facts === undefined) {
        return {
          rows: noRows,
          unavailable: 'Ownership unavailable — the root module did not elaborate',
        }
      }
      const matches = Analysis.ownershipMatchesOf(snapshot, snapshot.closure.rootModule)
      const loans = facts.functions.reduce((total, fn) => total + fn.loans.length, 0)
      return {
        rows: ownershipRows(facts),
        facts:
          matches.length === 0 && loans === 0
            ? undefined
            : [
                ...(matches.length === 0
                  ? []
                  : [{ text: `${matches.length} match lifetime`, tone: 'symbol' as const }]),
                ...(loans === 0
                  ? []
                  : [{ text: `${loans} lexical loan`, tone: 'symbol' as const }]),
              ],
        meta: `${facts.functions.length} fn${matches.length === 0 ? '' : ` · ${matches.length} match`}${loans === 0 ? '' : ` · ${loans} loan`}`,
      }
    },
  },
  {
    id: 'instances',
    title: 'Instances',
    phase: 'discovery',
    tag: 'INS',
    group: 'discovery',
    project: ({ snapshot }) => {
      const discovery = Analysis.instancesOf(snapshot)
      return {
        rows: instanceRows(discovery),
        meta: `${discovery.instances.length} inst`,
      }
    },
  },
  {
    id: 'layout',
    title: 'Target layout',
    phase: 'layout',
    tag: 'LAY',
    group: 'backend',
    project: ({ snapshot }) => {
      const catalog = Analysis.layoutCatalogOf(snapshot)
      const plan = Analysis.layoutOf(snapshot)
      const target = Analysis.targetOf(snapshot)
      const unions = Analysis.unionLayoutsOf(snapshot)
      const slices =
        plan._tag === 'Available'
          ? plan.value.entries.filter((entry) => entry.representation._tag === 'Slice')
          : []
      const rows = layoutRows(
        catalog._tag === 'Available' ? catalog.value : undefined,
        plan._tag === 'Available' ? plan.value : undefined,
        plan._tag === 'Available' ? undefined : plan.error.message,
      )
      const facts: ReadonlyArray<Fact> =
        target._tag === 'Resolved'
          ? [
              { text: target.target.id },
              {
                text: `${target.target.endianness} endian · ptr ${target.target.pointerSize} B`,
                tone: 'muted',
              },
              ...(unions.length === 0
                ? []
                : [{ text: `${unions.length} sum layout`, tone: 'symbol' as const }]),
              ...(slices.length === 0
                ? []
                : [{ text: `${slices.length} slice layout`, tone: 'symbol' as const }]),
            ]
          : [{ text: 'target unavailable', tone: 'warning' }]
      return { rows, facts, meta: plan._tag === 'Available' ? `${plan.value.entries.length}` : '—' }
    },
  },
  {
    id: 'mir',
    title: 'Control DAG',
    phase: 'lowering',
    tag: 'MIR',
    group: 'backend',
    // Lowering is target-aware, so MIR is absent whenever the target did not resolve; the pane
    // says which failure it was rather than rendering an empty graph.
    project: ({ snapshot }) => {
      const mir = Analysis.mirOf(snapshot)
      if (mir._tag !== 'Available') {
        return { rows: noRows, unavailable: `MIR unavailable — ${mir.error.message}` }
      }
      const regions = mir.value.functions.reduce((total, fn) => total + fn.regions.length, 0)
      const conversions = Analysis.mirUnionConversionsOf(snapshot)
      const matches = Analysis.mirMatchesOf(snapshot)
      const normalization = Analysis.effectNormalizationOf(snapshot)
      const normalized = normalization.filter((verdict) => verdict._tag === 'Normalized')
      const rejected = normalization.length - normalized.length
      return {
        rows: mirRows(mir.value),
        facts:
          conversions.length === 0 && matches.length === 0 && normalization.length === 0
            ? undefined
            : [
                ...(matches.length === 0
                  ? []
                  : [{ text: `${matches.length} structured match`, tone: 'symbol' as const }]),
                ...(conversions.length === 0
                  ? []
                  : [{ text: `${conversions.length} ConvertUnion`, tone: 'symbol' as const }]),
                ...(normalized.length === 0
                  ? []
                  : [
                      {
                        text: `${normalized.length} Effect normalization`,
                        tone: 'symbol' as const,
                      },
                    ]),
                ...(rejected === 0
                  ? []
                  : [{ text: `${rejected} normalization guard`, tone: 'warning' as const }]),
              ],
        meta: `${mir.value.functions.length} fn · ${regions} region${regions === 1 ? '' : 's'}${matches.length === 0 ? '' : ` · ${matches.length} match`}${conversions.length === 0 ? '' : ` · ${conversions.length} union`}${normalized.length === 0 ? '' : ` · ${normalized.length} normalized`}`,
      }
    },
  },
  {
    id: 'evaluation',
    title: 'Bootstrap evaluation',
    phase: 'interpretation',
    tag: 'EVA',
    group: 'semantics',
    // After MIR in pipeline order: the bootstrap interpreter runs the lowered program.
    action: { label: 'run' },
    project: ({ evaluation }) => {
      const rows = evaluationRows(evaluation)
      if (evaluation === undefined) return { rows, meta: 'not run' }
      const facts: ReadonlyArray<Fact> =
        evaluation._tag === 'Completed'
          ? [
              { text: 'Completed', tone: 'ok' },
              { text: `${evaluation.entry.name}() → ${evaluation.result.value}` },
              { text: `${evaluation.trace.length} steps`, tone: 'muted' },
            ]
          : evaluation._tag === 'UnhandledFailure'
            ? [
                { text: 'Unhandled failure', tone: 'warning' },
                { text: `${evaluation.report} · tag ${evaluation.tag}`, tone: 'muted' },
              ]
            : [
                { text: 'Blocked', tone: 'warning' },
                { text: evaluation.reason._tag, tone: 'muted' },
              ]
      return { rows, facts, meta: `${evaluation.trace.length} steps` }
    },
  },
  {
    id: 'backend',
    title: 'Backend output',
    phase: 'backend',
    tag: 'IR',
    group: 'backend',
    project: ({ snapshot, mode }) => {
      const emission = backendEmission(snapshot, mode)
      if (emission._tag === 'Rejected') {
        return {
          rows: noRows,
          facts: [{ text: 'Rejected', tone: 'warning' }],
          unavailable: `Emission rejected — ${emission.message}`,
        }
      }
      const artifact = emission.artifact
      const bytes = artifact._tag === 'LlvmBitcodeArtifact' ? artifact.bitcode : artifact.bytes
      const text = artifact._tag === 'LlvmBitcodeArtifact' ? artifact.ir : artifact.wat

      // Only a WebAssembly module can be run right here, so the execute-and-compare check is a
      // WebAssembly-target extra rather than something every backend can answer. The interpreter
      // ran the same MIR the backend emitted from, so a disagreement is a backend bug.
      const runnable = artifact._tag === 'WebAssemblyModuleArtifact'
      const execution = runnable ? execute(artifact.bytes) : undefined
      const interpreted =
        runnable && Analysis.mirOf(snapshot)._tag === 'Available'
          ? Analysis.evaluate(snapshot)
          : undefined
      const expected =
        interpreted === undefined
          ? undefined
          : interpreted._tag === 'Completed'
            ? interpreted.result.value
            : 'trap'
      const observed =
        execution === undefined
          ? undefined
          : execution._tag === 'Completed'
            ? execution.value
            : execution._tag === 'Trapped'
              ? 'trap'
              : 'failed'
      const agrees = observed !== undefined && observed === expected

      const executionRows: ReadonlyArray<RowModel> =
        execution === undefined
          ? []
          : [
              {
                key: 'exec-head',
                label: 'execution',
                detail: agrees
                  ? 'the backend and the interpreter agree'
                  : 'the backend and the interpreter disagree',
                head: true,
                tone: agrees ? 'ok' : 'error',
                dot: agrees ? 'ok' : 'error',
              },
              {
                key: 'exec-wasm',
                depth: 1,
                label: 'silk_main()',
                detail:
                  execution._tag === 'Completed'
                    ? `returned ${execution.value}`
                    : execution._tag === 'Trapped'
                      ? `trapped — ${execution.message}`
                      : `instantiation failed — ${execution.message}`,
              },
              {
                key: 'exec-interp',
                depth: 1,
                label: 'bootstrap interpreter',
                detail:
                  interpreted === undefined
                    ? 'not run — MIR unavailable'
                    : interpreted._tag === 'Completed'
                      ? `returned ${interpreted.result.value}`
                      : 'trapped',
              },
            ]

      return {
        rows: [
          ...symbolRows(artifact.symbols),
          ...executionRows,
          ...backendControlRows(artifact.control),
          ...backendTextRows(text),
          ...(runnable ? hexRows(bytes) : []),
        ],
        facts: [
          ...artifact.symbols.slice(0, 2).map(
            (entry): Fact => ({
              text: `${entry.symbol} → ${entry.declaration.module}.${entry.declaration.name}`,
              tone: 'symbol',
            }),
          ),
          {
            text: `${artifact.backend} · ${bytes.length} B · ${mode}`,
            tone: 'muted',
          },
          ...(execution === undefined
            ? []
            : [
                {
                  text: agrees ? 'runs · agrees' : 'runs · disagrees',
                  tone: agrees ? 'ok' : 'warning',
                } as Fact,
              ]),
        ],
        meta: `${bytes.length} B`,
      }
    },
  },
  {
    id: 'toolchain',
    title: 'Toolchain (planned)',
    phase: 'native',
    tag: 'TCH',
    group: 'backend',
    project: ({ snapshot, profile }) => {
      const commands = toolchainCommands(snapshot, profile)
      if (commands._tag === 'Unavailable') {
        return { rows: noRows, unavailable: `Toolchain unavailable — ${commands.message}` }
      }
      return {
        rows: toolchainRows(commands.commands),
        facts: [{ text: `clang · ${commands.target} · ${profile}`, tone: 'muted' }],
        meta: `${commands.commands.length} cmd`,
      }
    },
  },
  {
    id: 'pipeline',
    title: 'Pipeline',
    phase: 'all phases',
    tag: 'PIP',
    group: 'all',
    /**
     * How far the program got, one row per phase.
     *
     * Every other view is about a phase; this one is about the pipeline, which is why it is worth
     * a pane of its own — it is the fastest way to see which phase a program died in before
     * opening the pane that explains why.
     */
    project: ({ snapshot, profile }) => {
      const analysis = Analysis.rootAnalysis(snapshot)
      const discovery = Analysis.instancesOf(snapshot)
      const layout = Analysis.layoutOf(snapshot)
      const mir = Analysis.mirOf(snapshot)
      const ownership = Analysis.ownershipOf(snapshot, snapshot.closure.rootModule)
      const emission = backendEmission(snapshot, ToolchainPlan.codegenModeFor(profile))
      const countFor = (phase: string): number =>
        Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.phase === phase).length

      const phases = [
        {
          phase: 'syntax (lex + parse)',
          outputs: `${analysis.syntax.tokens.length} tokens · ${analysis.functions.length} declarations`,
          diagnostics: countFor('lexical') + countFor('parser'),
        },
        {
          phase: 'module closure',
          outputs: `${Analysis.modules(snapshot).length} modules · ${
            Analysis.cycles(snapshot).length
          } cycles`,
          diagnostics: countFor('module'),
        },
        {
          phase: 'declaration index',
          outputs: `${Analysis.declarationIndex(snapshot).modules.reduce(
            (sum, module) => sum + module.members.length,
            0,
          )} headers`,
          diagnostics: 0,
        },
        {
          phase: 'name resolution',
          outputs: `${Analysis.nameResolution(snapshot).modules.reduce(
            (sum, module) => sum + module.bindings.length,
            0,
          )} bindings`,
          diagnostics: countFor('semantic'),
        },
        {
          phase: 'elaboration (HIR)',
          outputs: `${analysis.hir.functions.length} typed functions`,
          diagnostics: 0,
        },
        {
          phase: 'ownership + cleanup',
          outputs: `${ownership?.functions.length ?? 0} checked functions`,
          diagnostics: countFor('ownership'),
        },
        {
          phase: 'instance discovery',
          outputs:
            discovery.entry._tag === 'Resolved'
              ? `${discovery.instances.length} instances`
              : `entry unavailable · ${discovery.entry.reason}`,
          diagnostics: 0,
        },
        {
          phase: 'target layout',
          outputs:
            layout._tag === 'Available'
              ? `${layout.value.target.id} · ${layout.value.entries.length} runtime entries`
              : layout.error.message,
          diagnostics: countFor('layout'),
        },
        {
          phase: 'MIR lowering',
          outputs:
            mir._tag === 'Available'
              ? `${mir.value.functions.length} lowered functions`
              : mir.error.message,
          diagnostics: 0,
        },
        {
          phase: 'backend',
          outputs:
            emission._tag === 'Emitted'
              ? emission.artifact._tag === 'LlvmBitcodeArtifact'
                ? `${emission.artifact.bitcode.length} bitcode bytes`
                : `${emission.artifact.bytes.length} WebAssembly bytes`
              : 'emission rejected',
          diagnostics: countFor('backend'),
        },
        { phase: 'native toolchain', outputs: 'object · shim · link', diagnostics: 0 },
      ]

      const blocked = phases.filter((entry) => entry.diagnostics > 0).length
      return {
        rows: pipelineRows(phases),
        meta: blocked === 0 ? `${phases.length} phases` : `${blocked} blocked`,
      }
    },
  },
  {
    id: 'diagnostics',
    title: 'Diagnostics',
    phase: 'all phases',
    tag: 'DX',
    group: 'all',
    project: ({ snapshot }) => {
      const analysis = Analysis.rootAnalysis(snapshot)
      const entries = diagnosticEntries(
        analysis.syntax.lexicalDiagnostics,
        analysis.syntax.parserDiagnostics,
        analysis.diagnostics,
      )
      const counts = diagnosticCounts(entries)
      return {
        rows: diagnosticRows(entries),
        meta: counts.errors === 0 ? 'clean' : `${counts.errors} err`,
      }
    },
  },
]

const byId = new Map(views.map((view) => [view.id, view]))

/**
 * Views that used to exist, and what replaced them.
 *
 * Layouts live in URLs and in localStorage, so a renamed view id outlives the rename: a link
 * shared before the backend panes merged still names `wasm`, and resolving it to a dead view
 * would turn a working link into an "unknown view" pane.
 */
const renamed: Readonly<Record<string, ViewId>> = { llvm: 'backend', wasm: 'backend' }

export const viewById = (id: string): ViewDefinition | undefined =>
  byId.get(id as ViewId) ?? byId.get(renamed[id] as ViewId)

/** Sibling phases offered as one-click tabs in a pane header. */
export const siblingsOf = (view: ViewDefinition): ReadonlyArray<ViewDefinition> =>
  views
    .filter((candidate) => candidate.group === view.group && candidate.id !== view.id)
    .slice(0, 2)
