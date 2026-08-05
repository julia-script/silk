'use client'

import { Mir } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const spanText = (span: { readonly start: number; readonly end: number }): string =>
  `[${span.start}, ${span.end})`

const provenanceText = (provenance: Mir.Provenance): string =>
  `${spanText(provenance.span)}${provenance.generated ? ' · generated' : ''}`

const localText = (local: Mir.LocalId): string => `%${local.ordinal}`

const operationLabel = (operation: Mir.Operation): string => {
  switch (operation._tag) {
    case 'Literal':
      return `${localText(operation.destination)} = literal ${operation.value}`
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
}: {
  readonly label: string
  readonly provenance: Mir.Provenance
}) {
  const reveal = provenanceText(provenance)
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

export function CfgView({ module }: { readonly module: Mir.Module }) {
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
                  />
                ))}
                <ProvenanceEntry
                  label={terminatorLabel(block.terminator)}
                  provenance={block.terminator.provenance}
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
  const module = samples[selected] ?? samples[0]

  if (module === undefined) return null
  return (
    <div>
      <div className={styles.exampleBar} aria-label="MIR samples">
        {samples.map((candidate, index) => (
          <button key={candidate.module} type="button" onClick={() => setSelected(index)}>
            {candidate.module.replace('sample://', '').replace('.silk', '')}
          </button>
        ))}
      </div>

      <CfgView module={module} />

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
