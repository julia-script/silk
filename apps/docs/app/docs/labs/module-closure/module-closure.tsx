'use client'

import { ModuleClosure } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import { DiagnosticPanel, diagnosticEntries } from '../syntax-inspector/syntax-inspector'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const mainFn = 'pub fn main() -> I32 { return 42 }'

interface Preset {
  readonly label: string
  readonly root: string
  readonly modules: Readonly<Record<string, string>>
}

const presets: ReadonlyArray<Preset> = [
  {
    label: 'Diamond',
    root: 'root',
    modules: {
      root: `import left\nimport right\n${mainFn}`,
      left: `import shared\n${mainFn}`,
      right: `import shared\n${mainFn}`,
      shared: mainFn,
    },
  },
  {
    label: 'Mutual cycle',
    root: 'root',
    modules: {
      root: `import beta\n${mainFn}`,
      beta: `import gamma\n${mainFn}`,
      gamma: `import beta\n${mainFn}`,
    },
  },
  {
    label: 'Unknown import',
    root: 'root',
    modules: {
      root: `import missing\n${mainFn}`,
    },
  },
  {
    label: 'Self import',
    root: 'root',
    modules: {
      root: `import root\n${mainFn}`,
    },
  },
  {
    label: 'Unreachable island',
    root: 'root',
    modules: {
      root: `import used\n${mainFn}`,
      used: mainFn,
      island: `import used\n${mainFn}`,
    },
  },
]

const encoder = new TextEncoder()

const spanLabel = (span: { readonly start: number; readonly end: number }): string =>
  `[${span.start}, ${span.end})`

const targetLabel = (target: ModuleClosure.ImportTarget): string => {
  switch (target._tag) {
    case 'Resolved':
      return `${target.module} · resolved ${spanLabel(target.token.span)}`
    case 'Unknown':
      return `${target.module} · unknown ${spanLabel(target.token.span)}`
    case 'Self':
      return `${target.module} · self ${spanLabel(target.token.span)}`
    case 'Unavailable':
      return `unavailable ${spanLabel(target.syntax.span)}`
  }
}

export function ClosureView({ closure }: { readonly closure: ModuleClosure.Closure }) {
  const cycleMembers = new Set(closure.cycles.flat())
  return (
    <div className={styles.diagnostics}>
      {closure.modules.map((module) => (
        <section
          key={module.name}
          className={styles.diagnosticGroup}
          aria-labelledby={`module-${module.name}`}
        >
          <div className={styles.diagnosticHeading}>
            <h3 id={`module-${module.name}`}>{module.name}</h3>
            <span>
              {module.name === closure.rootModule ? 'root' : 'module'}
              {cycleMembers.has(module.name) ? ' · cycle' : ''}
            </span>
          </div>
          {module.imports.length === 0 ? (
            <p className={styles.emptyState}>No imports</p>
          ) : (
            <ul className={styles.diagnosticList} aria-label={`Imports of ${module.name}`}>
              {module.imports.map((fact, index) => (
                <li key={`${module.name}-import-${index}`}>
                  <div>
                    <code>import</code>
                    <span>{targetLabel(fact.target)}</span>
                  </div>
                </li>
              ))}
            </ul>
          )}
        </section>
      ))}
      <section className={styles.diagnosticGroup} aria-labelledby="closure-cycles">
        <div className={styles.diagnosticHeading}>
          <h3 id="closure-cycles">Cycles</h3>
          <span>{closure.cycles.length}</span>
        </div>
        {closure.cycles.length === 0 ? (
          <p className={styles.emptyState}>No cycles</p>
        ) : (
          <ul className={styles.diagnosticList} aria-label="Import cycles">
            {closure.cycles.map((cycle) => (
              <li key={cycle.join('->')}>
                <div>
                  <code>cycle</code>
                  <span>{cycle.join(' → ')}</span>
                </div>
              </li>
            ))}
          </ul>
        )}
      </section>
    </div>
  )
}

export function ModuleClosureLab() {
  const [preset, setPreset] = useState<Preset>(presets[0] ?? { label: '', root: '', modules: {} })
  const [sources, setSources] = useState<Readonly<Record<string, string>>>(
    presets[0]?.modules ?? {},
  )
  const [selectedDiagnostic, setSelectedDiagnostic] = useState<number>()

  const closure = useMemo(
    () =>
      ModuleClosure.load({
        rootModule: preset.root,
        sources: new Map(Object.entries(sources).map(([name, text]) => [name, encoder.encode(text)])),
      }),
    [preset, sources],
  )

  return (
    <div>
      <div className={styles.exampleBar} aria-label="Closure presets">
        {presets.map((candidate) => (
          <button
            key={candidate.label}
            type="button"
            onClick={() => {
              setPreset(candidate)
              setSources(candidate.modules)
              setSelectedDiagnostic(undefined)
            }}
          >
            {candidate.label}
          </button>
        ))}
      </div>

      {Object.entries(sources).map(([name, text]) => (
        <div key={name}>
          <label htmlFor={`module-source-${name}`}>{name}</label>
          <textarea
            id={`module-source-${name}`}
            className={styles.editor}
            value={text}
            onChange={(event) => {
              setSources({ ...sources, [name]: event.target.value })
              setSelectedDiagnostic(undefined)
            }}
            spellCheck={false}
            autoCapitalize="off"
            autoCorrect="off"
          />
        </div>
      ))}

      <ClosureView closure={closure} />

      <div className={styles.diagnostics}>
        <DiagnosticPanel
          entries={diagnosticEntries(closure.diagnostics)}
          selected={selectedDiagnostic}
          onSelect={setSelectedDiagnostic}
        />
      </div>
    </div>
  )
}
