'use client'

import { Analysis } from '@silk-effect/compiler'
import type { NameResolution } from '@silk-effect/compiler'
import { useMemo, useState } from 'react'
import styles from '../syntax-inspector/syntax-inspector.module.css'

interface Preset {
  readonly label: string
  readonly root: string
  readonly modules: Readonly<Record<string, string>>
}

const presets: ReadonlyArray<Preset> = [
  {
    label: 'Namespace',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax\npub fn main() -> I32 { return Syntax.parse() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Selective alias',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { parse as read }\npub fn main() -> I32 { return read() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Hybrid alias',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax as Tree { parse }\npub fn main() -> I32 { return Tree.parse() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Private member',
    root: 'app/Main',
    modules: {
      'app/Main': 'import compiler.Syntax { hidden }\npub fn main() -> I32 { return 0 }',
      'compiler/Syntax': 'fn hidden() -> I32 { return 42 }',
    },
  },
  {
    label: 'Unknown member',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { missing }\npub fn main() -> I32 { return missing() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Damaged alias',
    root: 'app/Main',
    modules: {
      'app/Main': 'import compiler.Syntax as\npub fn main() -> I32 { return 0 }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Collision',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { parse }\npub fn parse() -> I32 { return 0 }\npub fn main() -> I32 { return parse() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Cycle',
    root: 'app/Main',
    modules: {
      'app/Main': 'import cycle.A { a }\npub fn main() -> I32 { return a() }',
      'cycle/A': 'import cycle.B { b }\npub fn a() -> I32 { return b() }',
      'cycle/B': 'import cycle.A { a }\npub fn b() -> I32 { return a() }',
    },
  },
]

const encoder = new TextEncoder()

const target = (binding: NameResolution.Binding): string => {
  if (binding._tag === 'Unavailable') {
    return binding.cause === undefined ? 'unavailable' : `unavailable · ${binding.cause.code}`
  }
  if (binding._tag === 'IntrinsicActor') return 'language intrinsic'
  if (binding._tag === 'ModuleNamespace') return binding.module
  return binding.declaration.canonical._tag === 'Canonical'
    ? `${binding.declaration.canonical.id.module}.${binding.declaration.canonical.id.name}`
    : 'unavailable declaration'
}

export function ResolutionView({
  resolution,
}: {
  readonly resolution: NameResolution.Resolution
}) {
  return (
    <div className={styles.diagnostics}>
      {resolution.modules.map((scope) => (
        <section
          className={styles.diagnosticGroup}
          key={scope.module}
          aria-labelledby={`scope-${scope.module}`}
        >
          <div className={styles.diagnosticHeading}>
            <h3 id={`scope-${scope.module}`}>{scope.module}</h3>
            <span>{scope.bindings.length} bindings</span>
          </div>
          <ul className={styles.diagnosticList} aria-label={`Bindings of ${scope.module}`}>
            {scope.bindings.map((binding, ordinal) => (
              <li key={`${binding.spelling}-${ordinal}`}>
                <div>
                  <code>{binding.spelling}</code>
                  <span>{binding._tag}</span>
                </div>
                <p>{target(binding)}</p>
              </li>
            ))}
          </ul>
          {scope.conflicts.map((conflict) => (
            <p key={conflict.spelling}>
              Conflict: {conflict.spelling} ({conflict.bindings.length} candidates)
            </p>
          ))}
        </section>
      ))}
    </div>
  )
}

export function NameResolutionLab() {
  const [preset, setPreset] = useState(
    presets[0] ?? { label: '', root: '', modules: {} },
  )
  const snapshot = useMemo(
    () =>
      Analysis.make({
        rootModule: preset.root,
        sources: new Map(
          Object.entries(preset.modules).map(([name, source]) => [
            name,
            encoder.encode(source),
          ]),
        ),
      }),
    [preset],
  )
  const calls = Analysis.modules(snapshot).flatMap(
    (module) =>
      Analysis.moduleAnalysis(snapshot, module.name)?.functions.flatMap((fn) => {
        const expression = fn.returnedExpression
        return expression._tag === 'Call' &&
          expression.reference._tag === 'Resolved' &&
          expression.reference.declaration.canonical._tag === 'Canonical'
          ? [
              {
                target: expression.reference.declaration.canonical.id,
                span: expression.syntax.span,
              },
            ]
          : []
      }) ?? [],
  )

  return (
    <div>
      <div className={styles.exampleBar} aria-label="Name resolution presets">
        {presets.map((candidate) => (
          <button key={candidate.label} type="button" onClick={() => setPreset(candidate)}>
            {candidate.label}
          </button>
        ))}
      </div>
      <ResolutionView resolution={Analysis.nameResolution(snapshot)} />
      <section aria-label="Canonical call references">
        <h3>Canonical call references</h3>
        {calls.length === 0 ? (
          <p>No resolved calls</p>
        ) : (
          <ul>
            {calls.map((call, ordinal) => (
              <li key={`${call.target.module}-${call.target.name}-${ordinal}`}>
                <code>{call.target.module}.{call.target.name}</code>{' '}
                <span>[{call.span.start}, {call.span.end})</span>
              </li>
            ))}
          </ul>
        )}
      </section>
      <section aria-label="Discovered instances">
        <h3>Discovered instances</h3>
        <ul>
          {Analysis.instancesOf(snapshot).instances.map((instance) => (
            <li
              key={`${instance.key.declaration.module}-${instance.key.declaration.name}`}
            >
              <code>{instance.key.declaration.module}.{instance.key.declaration.name}</code>
            </li>
          ))}
        </ul>
      </section>
      <section aria-label="Resolution diagnostics">
        <h3>Diagnostics</h3>
        {Analysis.diagnostics(snapshot).length === 0 ? (
          <p>No diagnostics</p>
        ) : (
          <ul>
            {Analysis.diagnostics(snapshot).map((diagnostic, ordinal) => (
              <li key={`${diagnostic.code}-${ordinal}`}>
                <code>{diagnostic.code}</code> {diagnostic.message}
              </li>
            ))}
          </ul>
        )}
      </section>
    </div>
  )
}
