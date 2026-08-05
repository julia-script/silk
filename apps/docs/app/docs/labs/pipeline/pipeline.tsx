'use client'

import { Analysis } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const sourceId = 'memory://docs/pipeline.silk'
const defaultSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

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
  const ownership = Analysis.ownershipOf(snapshot, rootModule)
  const phaseDiagnostics = (phase: string): number =>
    Analysis.diagnostics(snapshot).filter((diagnostic) => diagnostic.phase === phase).length

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
      phase: 'MIR lowering',
      lab: '/docs/labs/mir-cfg',
      outputs: `${Analysis.loweredMir(snapshot).functions.length} lowered functions`,
      diagnostics: 0,
    },
    {
      phase: 'backend (LLVM bitcode)',
      lab: '/docs/labs/llvm-ir',
      outputs: `${Analysis.codegen(snapshot, { mode: 'release' }).bitcode.length} bitcode bytes`,
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
    const built = Analysis.ofSource(sourceId, encoder.encode(text))
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
