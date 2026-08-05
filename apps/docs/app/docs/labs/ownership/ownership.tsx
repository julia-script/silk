'use client'

import { Analysis } from '@silk-effect/compiler'
import type { Ownership } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import { DiagnosticPanel, diagnosticEntries } from '../syntax-inspector/syntax-inspector'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const sourceId = 'memory://docs/ownership.silk'

const presets = [
  {
    label: 'Two parameters',
    source: `pub fn choose(left: I32, right: I32) -> I32 { return left }
pub fn main() -> I32 { return choose(1, 2) }`,
  },
  {
    label: 'Identity',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`,
  },
  {
    label: 'Damaged body',
    source: 'pub fn main() -> I32 { return missing() }',
  },
  {
    label: 'Unknown parameter type',
    source: 'pub fn puzzle(value: Mystery) -> I32 { return value }',
  },
] as const

const encoder = new TextEncoder()

const spanText = (span: { readonly start: number; readonly end: number }): string =>
  `[${span.start}, ${span.end})`

const functionLabel = (fn: Ownership.FunctionOwnership): string =>
  fn.declaration.name._tag === 'Present' ? fn.declaration.name.spelling : '∅'

export function OwnershipView({ facts }: { readonly facts: Ownership.ModuleOwnership }) {
  return (
    <div className={styles.diagnostics}>
      {facts.functions.map((fn) => (
        <section
          key={`ownership-${fn.declaration.id.ordinal}`}
          className={styles.diagnosticGroup}
          aria-labelledby={`ownership-${fn.declaration.id.ordinal}`}
        >
          <div className={styles.diagnosticHeading}>
            <h3 id={`ownership-${fn.declaration.id.ordinal}`}>{functionLabel(fn)}</h3>
            <span>{fn.verdict._tag === 'Satisfied' ? 'satisfied' : 'verdict unavailable'}</span>
          </div>
          <ul
            className={styles.diagnosticList}
            aria-label={`Binding timeline of ${functionLabel(fn)}`}
          >
            {fn.bindings.length === 0 ? (
              <li>
                <div>
                  <code>bindings</code>
                  <span>none</span>
                </div>
              </li>
            ) : (
              fn.bindings.map((binding) => (
                <li key={`binding-${binding.parameter.ordinal}`}>
                  <div>
                    <code>{binding.name ?? '∅'}</code>
                    <span>
                      {binding.category._tag.toLowerCase()} · live {spanText(binding.liveFrom)} →{' '}
                      {spanText(binding.liveTo)}
                    </span>
                  </div>
                </li>
              ))
            )}
            {fn.exits.map((exit, index) => (
              <li key={`exit-${index}`}>
                <div>
                  <code>exit {exit.kind.toLowerCase()}</code>
                  <span>
                    {spanText(exit.span)} · releases{' '}
                    {exit.releases.length === 0
                      ? 'none'
                      : exit.releases.map((release) => `p${release.binding.parameter.ordinal}`).join(', ')}
                  </span>
                </div>
              </li>
            ))}
          </ul>
        </section>
      ))}
    </div>
  )
}

export function OwnershipLab() {
  const [text, setText] = useState<string>(presets[0].source)
  const [selectedDiagnostic, setSelectedDiagnostic] = useState<number>()

  const snapshot = useMemo(() => Analysis.ofSource(sourceId, encoder.encode(text)), [text])
  const facts = Analysis.ownershipOf(snapshot, sourceId)

  return (
    <div>
      <div className={styles.exampleBar} aria-label="Ownership presets">
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

      <label className="sr-only" htmlFor="ownership-source">
        Silk source code
      </label>
      <textarea
        id="ownership-source"
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

      {facts === undefined ? null : <OwnershipView facts={facts} />}

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
