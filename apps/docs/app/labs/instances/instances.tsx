'use client'

import { Analysis } from '@silk-effect/compiler'
import * as Snapshot from '../snapshot'
import type { Instances } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import { DiagnosticPanel, diagnosticEntries } from '../syntax-inspector/syntax-inspector'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const sourceId = 'memory/docs/instances'

const presets = [
  {
    label: 'Nested calls',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  },
  {
    label: 'Mutual recursion',
    source: `pub fn main() -> I32 { return other() }
pub fn other() -> I32 { return main() }`,
  },
  {
    label: 'Unreachable declaration',
    source: `pub fn unused() -> I32 { return 1 }
pub fn main() -> I32 { return 42 }`,
  },
  {
    label: 'Missing entry',
    source: 'pub fn answer() -> I32 { return 42 }',
  },
] as const

const encoder = new TextEncoder()

const keyLabel = (key: Instances.InstanceKey): string =>
  `${key.declaration.module}.${key.declaration.name}<${key.typeArguments.join(', ')}>[${key.contractRow.join(', ')}]`

const entryLabel = (entry: Instances.Entry): string =>
  entry._tag === 'Resolved' ? `resolved · ${entry.key.declaration.name}` : `unavailable · ${entry.reason}`

export function DiscoveryView({ discovery }: { readonly discovery: Instances.Discovery }) {
  return (
    <div className={styles.diagnostics}>
      <section className={styles.diagnosticGroup} aria-labelledby="discovery-entry">
        <div className={styles.diagnosticHeading}>
          <h3 id="discovery-entry">Entry</h3>
          <span>{entryLabel(discovery.entry)}</span>
        </div>
        {discovery.instances.length === 0 ? (
          <p className={styles.emptyState}>No instances</p>
        ) : (
          <ul className={styles.diagnosticList} aria-label="Discovered instances">
            {discovery.instances.map((instance, index) => (
              <li key={keyLabel(instance.key)}>
                <div>
                  <code>#{index}</code>
                  <span>{keyLabel(instance.key)}</span>
                </div>
              </li>
            ))}
          </ul>
        )}
      </section>
    </div>
  )
}

export function InstancesLab() {
  const [text, setText] = useState<string>(presets[0].source)
  const [selectedDiagnostic, setSelectedDiagnostic] = useState<number>()

  const snapshot = useMemo(() => Snapshot.ofSource(sourceId, encoder.encode(text)), [text])
  const discovery = Analysis.instancesOf(snapshot)

  return (
    <div>
      <div className={styles.exampleBar} aria-label="Instance presets">
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

      <label className="sr-only" htmlFor="instances-source">
        Silk source code
      </label>
      <textarea
        id="instances-source"
        className={styles.editor}
        value={text}
        onChange={(event) => {
          setText(event.target.value)
          setSelectedDiagnostic(undefined)
        }}
        spellCheck={false}
        autoCapitalize="off"
        autoCorrect="off"
      />

      <DiscoveryView discovery={discovery} />

      <div className={styles.diagnostics}>
        <DiagnosticPanel
          entries={diagnosticEntries(Analysis.diagnostics(snapshot))}
          selected={selectedDiagnostic}
          onSelect={setSelectedDiagnostic}
        />
      </div>
    </div>
  )
}
