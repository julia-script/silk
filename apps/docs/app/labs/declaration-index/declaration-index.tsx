'use client'

import { Analysis } from '@silk-effect/compiler'
import type { DeclarationIndex } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import { DiagnosticPanel, diagnosticEntries } from '../syntax-inspector/syntax-inspector'
import styles from '../syntax-inspector/syntax-inspector.module.css'

interface Preset {
  readonly label: string
  readonly root: string
  readonly modules: Readonly<Record<string, string>>
}

const presets: ReadonlyArray<Preset> = [
  {
    label: 'Two modules',
    root: 'root',
    modules: {
      root: 'import lib\npub fn main() -> I32 { return 42 }',
      lib: 'pub fn answer() -> I32 { return 1 }\npub fn choose(left: I32, right: I32) -> I32 { return left }',
    },
  },
  {
    label: 'Same name twice across modules',
    root: 'root',
    modules: {
      root: 'import lib\npub fn answer() -> I32 { return 1 }',
      lib: 'pub fn answer() -> I32 { return 2 }',
    },
  },
  {
    label: 'Duplicate in one module',
    root: 'root',
    modules: {
      root: 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }',
    },
  },
  {
    label: 'Missing name',
    root: 'root',
    modules: {
      root: 'pub fn () -> I32 { return 0 }',
    },
  },
  {
    label: 'Unknown types',
    root: 'root',
    modules: {
      root: 'pub fn puzzle(value: Mystery) -> Enigma { return 0 }',
    },
  },
]

const encoder = new TextEncoder()

const typeLabel = (fact: DeclarationIndex.DeclaredTypeFact): string =>
  fact._tag === 'Resolved' ? fact.type : fact._tag === 'Unresolved' ? `${fact.spelling}?` : '∅'

const signatureLabel = (declaration: DeclarationIndex.DeclarationFact): string => {
  const parameters = declaration.parameters
    .map((parameter) =>
      `${parameter.name._tag === 'Present' ? parameter.name.spelling : '∅'}: ${typeLabel(parameter.declaredType)}`,
    )
    .join(', ')
  return `(${parameters}) -> ${typeLabel(declaration.returnType)}`
}

const canonicalLabel = (state: DeclarationIndex.CanonicalState): string => {
  switch (state._tag) {
    case 'Canonical':
      return `${state.id.module}.${state.id.name}`
    case 'Duplicate':
      return `duplicate of ${state.original.module}.${state.original.name}`
    case 'Unidentified':
      return 'unidentified'
  }
}

export function IndexView({ index }: { readonly index: DeclarationIndex.Index }) {
  return (
    <div className={styles.diagnostics}>
      {index.modules.map((module) => (
        <section
          key={module.module}
          className={styles.diagnosticGroup}
          aria-labelledby={`index-${module.module}`}
        >
          <div className={styles.diagnosticHeading}>
            <h3 id={`index-${module.module}`}>{module.module}</h3>
            <span>{module.declarations.length}</span>
          </div>
          {module.declarations.length === 0 ? (
            <p className={styles.emptyState}>No declarations</p>
          ) : (
            <ul className={styles.diagnosticList} aria-label={`Headers of ${module.module}`}>
              {module.declarations.map((declaration) => (
                <li key={`${module.module}-${declaration.id.ordinal}`}>
                  <div>
                    <code>
                      {declaration.name._tag === 'Present' ? declaration.name.spelling : '∅'}
                    </code>
                    <span>{canonicalLabel(declaration.canonical)}</span>
                  </div>
                  <p>{signatureLabel(declaration)}</p>
                </li>
              ))}
            </ul>
          )}
        </section>
      ))}
    </div>
  )
}

export function DeclarationIndexLab() {
  const [preset, setPreset] = useState<Preset>(presets[0] ?? { label: '', root: '', modules: {} })
  const [sources, setSources] = useState<Readonly<Record<string, string>>>(
    presets[0]?.modules ?? {},
  )
  const [selectedDiagnostic, setSelectedDiagnostic] = useState<number>()

  const index = useMemo(
    () =>
      Analysis.declarationIndex(
        Analysis.make({
          rootModule: preset.root,
          sources: new Map(
            Object.entries(sources).map(([name, text]) => [name, encoder.encode(text)]),
          ),
        }),
      ),
    [preset, sources],
  )

  return (
    <div>
      <div className={styles.exampleBar} aria-label="Index presets">
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
          <label htmlFor={`index-source-${name}`}>{name}</label>
          <textarea
            id={`index-source-${name}`}
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

      <IndexView index={index} />

      <div className={styles.diagnostics}>
        <DiagnosticPanel
          entries={diagnosticEntries(index.diagnostics)}
          selected={selectedDiagnostic}
          onSelect={setSelectedDiagnostic}
        />
      </div>
    </div>
  )
}
