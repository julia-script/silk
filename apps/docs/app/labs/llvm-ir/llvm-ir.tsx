'use client'

import { Analysis } from '@silk-effect/compiler'
import * as Effect from 'effect/Effect'
import { useMemo, useState } from 'react'
import { CfgView } from '../mir-cfg/mir-cfg'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const sourceId = 'memory/docs/llvm-ir'

const presets = [
  {
    label: 'Nested calls',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  },
  {
    label: 'Two parameters',
    source: `pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(1, 42) }`,
  },
  {
    label: 'Operator pipeline',
    source: 'pub fn main() -> I32 { return 2 + 3 * 4 |> I32.add(1) }',
  },
  {
    label: 'Negation trap',
    source: 'pub fn main() -> I32 { return -(-2147483648) }',
  },
  {
    label: 'Trap body',
    source: 'pub fn main() -> I32 { return missing() }',
  },
] as const

const encoder = new TextEncoder()

export function LlvmIrLab() {
  const [text, setText] = useState<string>(presets[0].source)
  const [mode, setMode] = useState<'release' | 'debug'>('release')

  const snapshot = useMemo(
    () => Analysis.ofSource(sourceId, encoder.encode(text), 'aarch64-apple-darwin'),
    [text],
  )
  const artifact = useMemo(
    () => Effect.runSync(Analysis.codegen(snapshot, { mode })),
    [snapshot, mode],
  )
  const layout = Analysis.layoutOf(snapshot)

  return (
    <div>
      <div className={styles.exampleBar} aria-label="LLVM IR presets">
        {presets.map((preset) => (
          <button key={preset.label} type="button" onClick={() => setText(preset.source)}>
            {preset.label}
          </button>
        ))}
        <button
          type="button"
          onClick={() => setMode(mode === 'release' ? 'debug' : 'release')}
          aria-pressed={mode === 'debug'}
        >
          {mode === 'release' ? 'debug metadata: off' : 'debug metadata: on'}
        </button>
      </div>

      <label className="sr-only" htmlFor="llvm-ir-source">
        Silk source code
      </label>
      <textarea
        id="llvm-ir-source"
        className={styles.editor}
        value={text}
        onChange={(event) => setText(event.target.value)}
        spellCheck={false}
        autoCapitalize="off"
        autoCorrect="off"
      />

      <div className={styles.diagnostics}>
        {layout._tag === 'Available' ? (
          <section className={styles.diagnosticGroup} aria-labelledby="llvm-target-layout">
            <div className={styles.diagnosticHeading}>
              <h3 id="llvm-target-layout">Compiler target and scalar plan</h3>
              <span>{layout.value.target.id}</span>
            </div>
            <ul className={styles.diagnosticList} aria-label="LLVM target layout plan">
              {layout.value.entries.map((entry) => (
                <li key={entry.type}>
                  <div>
                    <code>{entry.type}</code>
                    <span>
                      {entry.size} bytes · align {entry.alignment} · LLVM i
                      {entry.representation.bits}
                    </span>
                  </div>
                </li>
              ))}
            </ul>
          </section>
        ) : null}
        <section className={styles.diagnosticGroup} aria-labelledby="llvm-symbols">
          <div className={styles.diagnosticHeading}>
            <h3 id="llvm-symbols">Symbols</h3>
            <span>{artifact.symbols.length}</span>
          </div>
          <ul className={styles.diagnosticList} aria-label="Backend symbol table">
            {artifact.symbols.map((entry) => (
              <li key={entry.symbol}>
                <div>
                  <code>{entry.symbol}</code>
                  <span>
                    {entry.declaration.module}.{entry.declaration.name}
                  </span>
                </div>
              </li>
            ))}
          </ul>
        </section>
      </div>

      <section className={styles.diagnosticGroup} aria-labelledby="llvm-ir-text">
        <div className={styles.diagnosticHeading}>
          <h3 id="llvm-ir-text">Emitted LLVM IR ({mode})</h3>
          <span>{artifact.bitcode.length} bitcode bytes</span>
        </div>
        <pre aria-label="Emitted LLVM IR text">{artifact.ir}</pre>
      </section>

      <CfgView module={Analysis.loweredMir(snapshot)} source={text} />
    </div>
  )
}
