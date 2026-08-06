'use client'

import { Analysis, Mir } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const programSourceId = 'memory/docs/mir-program'
const defaultProgram = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

const typeText = (type: 'I32' | 'Bool' | { readonly module: string; readonly name: string }): string =>
  typeof type === 'string' ? type : `${type.module}.${type.name}`

const spanText = (span: { readonly start: number; readonly end: number }): string =>
  `[${span.start}, ${span.end})`

const provenanceText = (provenance: Mir.Provenance): string =>
  `${spanText(provenance.span)}${provenance.generated ? ' · generated' : ''}`

const localText = (local: Mir.LocalId): string => `%${local.ordinal}`

const operationLabel = (operation: Mir.Operation): string => {
  switch (operation._tag) {
    case 'Literal':
      return `${localText(operation.destination)} = literal ${operation.value}`
    case 'Binary':
      return `${localText(operation.destination)} = ${operation.operator.toLowerCase()} ${localText(operation.left)}, ${localText(operation.right)}`
    case 'Move':
      return `${localText(operation.destination)} = move ${localText(operation.source)}`
    case 'Call':
      return `${localText(operation.destination)} = call ${operation.target.name}(${operation.arguments.map(localText).join(', ')})`
    case 'Drop':
      return `drop ${localText(operation.local)}`
  }
}

const terminatorLabel = (terminator: Mir.Terminator): string => {
  switch (terminator._tag) {
    case 'Return':
      return `return ${localText(terminator.value)}`
    case 'Jump':
      return `jump bb${terminator.target.ordinal}`
    case 'Branch':
      return `branch ${localText(terminator.condition)} ? bb${terminator.taken.ordinal} : bb${terminator.otherwise.ordinal}`
    case 'Trap':
      return `trap "${terminator.reason}"`
  }
}

const edgeLabels = (terminator: Mir.Terminator): ReadonlyArray<string> => {
  switch (terminator._tag) {
    case 'Jump':
      return [`→ bb${terminator.target.ordinal}`]
    case 'Branch':
      return [`→ bb${terminator.taken.ordinal}`, `→ bb${terminator.otherwise.ordinal}`]
    default:
      return []
  }
}

function ProvenanceEntry({
  label,
  provenance,
  source,
}: {
  readonly label: string
  readonly provenance: Mir.Provenance
  readonly source?: string
}) {
  const slice =
    source === undefined ? '' : ` · "${source.slice(provenance.span.start, provenance.span.end)}"`
  const reveal = `${provenanceText(provenance)}${slice}`
  return (
    <li>
      <div>
        <code tabIndex={0} title={reveal} aria-label={`${label} : ${reveal}`}>
          {label}
        </code>
      </div>
    </li>
  )
}

export function CfgView({
  module,
  source,
}: {
  readonly module: Mir.Module
  readonly source?: string
}) {
  return (
    <div>
      {module.functions.map((fn) => (
        <div key={fn.id.name} className={styles.diagnostics}>
          {fn.blocks.map((block) => (
            <section
              key={`${fn.id.name}-bb${block.id.ordinal}`}
              className={styles.diagnosticGroup}
              aria-labelledby={`cfg-${fn.id.name}-bb${block.id.ordinal}`}
            >
              <div className={styles.diagnosticHeading}>
                <h3 id={`cfg-${fn.id.name}-bb${block.id.ordinal}`}>
                  {fn.id.name} · bb{block.id.ordinal}
                  {block.kind === 'Cleanup' ? ' · cleanup' : ''}
                </h3>
                <span>{edgeLabels(block.terminator).join(' ') || 'exit'}</span>
              </div>
              <ul
                className={styles.diagnosticList}
                aria-label={`Operations of ${fn.id.name} bb${block.id.ordinal}`}
              >
                {block.operations.map((operation, index) => (
                  <ProvenanceEntry
                    key={`op-${index}`}
                    label={operationLabel(operation)}
                    provenance={operation.provenance}
                    source={source}
                  />
                ))}
                <ProvenanceEntry
                  label={terminatorLabel(block.terminator)}
                  provenance={block.terminator.provenance}
                  source={source}
                />
              </ul>
            </section>
          ))}
        </div>
      ))}
    </div>
  )
}

export function MirCfgLab() {
  const samples = useMemo(() => Mir.samples(), [])
  const [selected, setSelected] = useState(0)
  const [text, setText] = useState<string>(defaultProgram)
  const snapshot = useMemo(
    () =>
      Analysis.ofSource(programSourceId, new TextEncoder().encode(text), 'aarch64-apple-darwin'),
    [text],
  )
  const program = useMemo(() => Analysis.loweredMir(snapshot), [snapshot])
  const module = selected < samples.length ? samples[selected] : program
  const isProgram = selected >= samples.length

  if (module === undefined) return null
  return (
    <div>
      <div className={styles.exampleBar} aria-label="MIR samples">
        {samples.map((candidate, index) => (
          <button key={candidate.module} type="button" onClick={() => setSelected(index)}>
            {candidate.module.replace('sample://', '').replace('', '')}
          </button>
        ))}
        <button type="button" onClick={() => setSelected(samples.length)}>
          lowered program
        </button>
      </div>

      {isProgram ? (
        <>
          <label className="sr-only" htmlFor="mir-program-source">
            Silk source code
          </label>
          <textarea
            id="mir-program-source"
            className={styles.editor}
            value={text}
            onChange={(event) => setText(event.target.value)}
            spellCheck={false}
            autoCapitalize="off"
            autoCorrect="off"
          />
        </>
      ) : null}

      <CfgView module={module} source={isProgram ? text : undefined} />

      <section className={styles.diagnosticGroup} aria-labelledby="mir-target-layout">
        <div className={styles.diagnosticHeading}>
          <h3 id="mir-target-layout">Canonical target and layout</h3>
          <span>{module.layout.target.id}</span>
        </div>
        <ul className={styles.diagnosticList} aria-label="MIR target layout plan">
          {module.layout.entries.map((entry) => (
            <li key={typeText(entry.type)}>
              <div>
                <code>{typeText(entry.type)}</code>
                <span>
                  {entry.size} bytes · align {entry.alignment} · {entry.representation._tag}
                </span>
              </div>
            </li>
          ))}
        </ul>
      </section>

      <section className={styles.diagnosticGroup} aria-labelledby="mir-encoding">
        <div className={styles.diagnosticHeading}>
          <h3 id="mir-encoding">Encoded MIR</h3>
          <span>{module.module}</span>
        </div>
        <pre aria-label="Deterministic MIR encoding">{Mir.encode(module)}</pre>
      </section>
    </div>
  )
}
