'use client'

import { Analysis, ToolchainPlan } from '@silk-effect/compiler'
import * as Snapshot from '../snapshot'
import * as Effect from 'effect/Effect'
import { useMemo, useState } from 'react'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const sourceId = 'memory/docs/toolchain'
const defaultSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

const encoder = new TextEncoder()
const clang = '/usr/bin/clang'

const commandText = (planned: ToolchainPlan.PlannedCommand): string =>
  [planned.command, ...planned.arguments].join(' ')

export function ToolchainLab() {
  const [text, setText] = useState<string>(defaultSource)
  const [profile, setProfile] = useState<ToolchainPlan.OptimizationProfile>('release')

  const snapshot = useMemo(
    () => Snapshot.ofSource(sourceId, encoder.encode(text), 'aarch64-apple-darwin'),
    [text],
  )
  const artifact = useMemo(
    () =>
      Effect.runSync(
        Analysis.codegen(snapshot, { mode: ToolchainPlan.codegenModeFor(profile) }),
      ),
    [snapshot, profile],
  )
  const selected = Analysis.targetOf(snapshot)
  if (selected._tag === 'Unavailable') return null
  const target = selected.target

  const objectPlan = ToolchainPlan.objectCommand(
    clang,
    target,
    profile,
    '<scope>/program.bc',
    '<scope>/program.o',
  )
  const shimPlan = ToolchainPlan.shimCommand(
    clang,
    target,
    '<scope>/silk_shim.c',
    '<scope>/silk_shim.o',
  )
  const linkPlan = ToolchainPlan.linkCommand(
    clang,
    target,
    ['<scope>/program.o', '<scope>/silk_shim.o'],
    [],
    '<destination>/program',
  )

  return (
    <div>
      <div className={styles.exampleBar} aria-label="Optimization profiles">
        {(['debug', 'release', 'release-with-debug'] as const).map((candidate) => (
          <button
            key={candidate}
            type="button"
            onClick={() => setProfile(candidate)}
            aria-pressed={profile === candidate}
          >
            {candidate}
          </button>
        ))}
      </div>

      <label className="sr-only" htmlFor="toolchain-source">
        Silk source code
      </label>
      <textarea
        id="toolchain-source"
        className={styles.editor}
        value={text}
        onChange={(event) => setText(event.target.value)}
        spellCheck={false}
        autoCapitalize="off"
        autoCorrect="off"
      />

      <div className={styles.diagnostics}>
        <section className={styles.diagnosticGroup} aria-labelledby="toolchain-commands">
          <div className={styles.diagnosticHeading}>
            <h3 id="toolchain-commands">Planned commands · {profile}</h3>
            <span>{artifact.bitcode.length} bitcode bytes</span>
          </div>
          <ul className={styles.diagnosticList} aria-label="Planned toolchain commands">
            <li>
              <div>
                <code>object</code>
                <span>{commandText(objectPlan)}</span>
              </div>
            </li>
            <li>
              <div>
                <code>shim</code>
                <span>{commandText(shimPlan)}</span>
              </div>
            </li>
            <li>
              <div>
                <code>link</code>
                <span>{commandText(linkPlan)}</span>
              </div>
            </li>
          </ul>
        </section>

        <section className={styles.diagnosticGroup} aria-labelledby="toolchain-scope">
          <div className={styles.diagnosticHeading}>
            <h3 id="toolchain-scope">Build scope</h3>
            <span>owned intermediates</span>
          </div>
          <ul className={styles.diagnosticList} aria-label="Build scope lifecycle">
            <li>
              <div>
                <code>1</code>
                <span>scope opens: a named temporary directory owns every intermediate</span>
              </div>
            </li>
            <li>
              <div>
                <code>2</code>
                <span>program.bc and program.o are written as path-backed scope artifacts</span>
              </div>
            </li>
            <li>
              <div>
                <code>3</code>
                <span>only an explicit save-temps promotion copies an artifact out durably</span>
              </div>
            </li>
            <li>
              <div>
                <code>4</code>
                <span>scope exit removes the directory after success or failure alike</span>
              </div>
            </li>
          </ul>
        </section>
      </div>

      <section className={styles.diagnosticGroup} aria-labelledby="toolchain-shim">
        <div className={styles.diagnosticHeading}>
          <h3 id="toolchain-shim">Runtime shim</h3>
          <span>private, compiler-versioned</span>
        </div>
        <pre aria-label="Runtime shim source">{ToolchainPlan.shimSource}</pre>
      </section>
    </div>
  )
}
