'use client'

import { Analysis } from '@silk-effect/compiler'
import * as Snapshot from '../snapshot'
import * as Effect from 'effect/Effect'
import { useMemo, useState } from 'react'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const sourceId = 'memory/docs/pipeline'
const defaultSource = 'pub fn main() -> I32 { return 2 + 3 * 4 |> I32.add(1) }'

const encoder = new TextEncoder()

export interface PhaseRow {
  readonly phase: string
  readonly lab: string | undefined
  readonly outputs: string
  readonly diagnostics: number
}

export const pipelineRows = (snapshot: Analysis.Snapshot): ReadonlyArray<PhaseRow> => {
  const rootModule = snapshot.closure.rootModule
  const analysis = Analysis.rootAnalysis(snapshot)
  const discovery = Analysis.instancesOf(snapshot)
  const layout = Analysis.layoutOf(snapshot)
  const ownership = Analysis.ownershipOf(snapshot, rootModule)
  const phaseDiagnostics = (phase: string): number =>
    Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.phase === phase).length
  const backendOutput = (() => {
    try {
      return `${Effect.runSync(Analysis.codegen(snapshot, { mode: 'release' })).bitcode.length} bitcode bytes`
    } catch {
      return 'emission unavailable'
    }
  })()

  return [
    {
      phase: 'syntax (lex + parse)',
      lab: '/docs/labs/syntax-inspector',
      outputs: `${analysis.syntax.tokens.length} tokens · ${analysis.functions.length} declarations`,
      diagnostics: phaseDiagnostics('lexical') + phaseDiagnostics('parser'),
    },
    {
      phase: 'module closure',
      lab: '/docs/labs/module-closure',
      outputs: `${Analysis.modules(snapshot).length} modules · ${Analysis.cycles(snapshot).length} cycles`,
      diagnostics: phaseDiagnostics('module'),
    },
    {
      phase: 'declaration index',
      lab: '/docs/labs/declaration-index',
      outputs: `${Analysis.declarationIndex(snapshot).modules.reduce(
        (sum, module) => sum + module.declarations.length,
        0,
      )} headers`,
      diagnostics: 0,
    },
    {
      phase: 'name resolution',
      lab: '/docs/labs/name-resolution',
      outputs: `${Analysis.nameResolution(snapshot).modules.reduce((sum, module) => sum + module.bindings.length, 0)} bindings`,
      diagnostics: phaseDiagnostics('semantic'),
    },
    {
      phase: 'elaboration (HIR)',
      lab: '/docs/labs/syntax-inspector',
      outputs: `${analysis.hir.functions.length} typed functions`,
      diagnostics: phaseDiagnostics('semantic'),
    },
    {
      phase: 'ownership + cleanup plan',
      lab: '/docs/labs/ownership',
      outputs: `${ownership?.functions.length ?? 0} checked functions`,
      diagnostics: 0,
    },
    {
      phase: 'instance discovery',
      lab: '/docs/labs/instances',
      outputs:
        discovery.entry._tag === 'Resolved'
          ? `${discovery.instances.length} instances`
          : `entry unavailable · ${discovery.entry.reason}`,
      diagnostics: 0,
    },
    {
      phase: 'target layout',
      lab: '/docs/labs/mir-cfg',
      outputs:
        layout._tag === 'Available'
          ? `${layout.value.target.id} · ${layout.value.entries.length} scalar entries`
          : layout.error.message,
      diagnostics: 0,
    },
    {
      phase: 'MIR lowering',
      lab: '/docs/labs/mir-cfg',
      outputs: `${Analysis.loweredMir(snapshot).functions.length} lowered functions`,
      diagnostics: 0,
    },
    {
      phase: 'backend (LLVM bitcode)',
      lab: '/docs/labs/llvm-ir',
      outputs: backendOutput,
      diagnostics: 0,
    },
    {
      phase: 'native toolchain (planned)',
      lab: '/docs/labs/toolchain',
      outputs: 'object · shim · link',
      diagnostics: 0,
    },
  ]
}

export function PipelineLab() {
  const [text, setText] = useState<string>(defaultSource)

  const { snapshot, elapsedMs } = useMemo(() => {
    const startedAt = performance.now()
    const built = Snapshot.ofSource(sourceId, encoder.encode(text), 'aarch64-apple-darwin')
    return { snapshot: built, elapsedMs: performance.now() - startedAt }
  }, [text])
  const rows = pipelineRows(snapshot)

  return (
    <div>
      <label className="sr-only" htmlFor="pipeline-source">
        Silk source code
      </label>
      <textarea
        id="pipeline-source"
        className={styles.editor}
        value={text}
        onChange={(event) => setText(event.target.value)}
        spellCheck={false}
        autoCapitalize="off"
        autoCorrect="off"
      />

      <section className={styles.diagnosticGroup} aria-labelledby="pipeline-overview">
        <div className={styles.diagnosticHeading}>
          <h3 id="pipeline-overview">Pipeline</h3>
          <span suppressHydrationWarning>snapshot built in {elapsedMs.toFixed(1)} ms</span>
        </div>
        <ul className={styles.diagnosticList} aria-label="Pipeline phases">
          {rows.map((row) => (
            <li key={row.phase}>
              <div>
                <code>{row.phase}</code>
                <span>
                  {row.outputs} ·{' '}
                  {row.diagnostics === 0 ? 'no diagnostics' : `${row.diagnostics} diagnostics`}
                </span>
              </div>
              {row.lab === undefined ? null : (
                <p>
                  <a href={row.lab}>open lab</a>
                </p>
              )}
            </li>
          ))}
        </ul>
      </section>
    </div>
  )
}
