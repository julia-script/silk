'use client'

import { Analysis } from '@silk-effect/compiler'
import * as Snapshot from '../snapshot'
import * as Effect from 'effect/Effect'
import { useMemo, useState } from 'react'
import { CfgView } from '../mir-cfg/mir-cfg'
import styles from '../syntax-inspector/syntax-inspector.module.css'

const sourceId = 'memory/docs/wasm'

const presets = [
  {
    label: 'Nested calls',
    source: `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`,
  },
  {
    label: 'Branch diamond',
    source: 'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }',
  },
  {
    label: 'Checked arithmetic',
    source: 'pub fn main() -> I32 { return I32.divide(I32.add(40, 2), 1) }',
  },
  {
    label: 'Operator pipeline',
    source: 'pub fn main() -> I32 { return 2 + 3 * 4 |> I32.add(1) }',
  },
  {
    label: 'Operator condition',
    source: 'pub fn main() -> I32 { if !(1 == 2) { return 42 } return 0 }',
  },
  {
    label: 'Overflow traps',
    source: 'pub fn main() -> I32 { return I32.add(2147483647, 1) }',
  },
  {
    label: 'Divide by zero traps',
    source: 'pub fn main() -> I32 { return I32.divide(1, 0) }',
  },
] as const

const encoder = new TextEncoder()

/** The outcome of instantiating the emitted module and calling its entry export. */
type Execution =
  | { readonly _tag: 'Completed'; readonly value: number }
  | { readonly _tag: 'Trapped'; readonly message: string }
  | { readonly _tag: 'Failed'; readonly message: string }

const messageOf = (error: unknown): string =>
  error instanceof Error ? error.message : String(error)

/**
 * Runs the emitted binary in the browser's own WebAssembly engine, which doubles as an
 * independent validator: a module the engine rejects never reaches the call.
 */
const execute = (bytes: Uint8Array): Execution => {
  let main: () => number
  try {
    // `slice` re-backs the bytes with a plain `ArrayBuffer`, which is what the WebAssembly types
    // accept; the artifact's own array is generic over `ArrayBufferLike`.
    const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes.slice()), {})
    main = instance.exports.silk_main as () => number
  } catch (error) {
    return { _tag: 'Failed', message: messageOf(error) }
  }
  try {
    return { _tag: 'Completed', value: main() }
  } catch (error) {
    return { _tag: 'Trapped', message: messageOf(error) }
  }
}

const hex = (bytes: Uint8Array): string => {
  const lines: Array<string> = []
  for (let offset = 0; offset < bytes.length; offset += 16) {
    const row = Array.from(bytes.slice(offset, offset + 16), (byte) =>
      byte.toString(16).padStart(2, '0'),
    )
    lines.push(`${offset.toString(16).padStart(8, '0')}  ${row.join(' ')}`)
  }
  return lines.join('\n')
}

export function WasmLab() {
  const [text, setText] = useState<string>(presets[0].source)
  const [mode, setMode] = useState<'release' | 'debug'>('debug')

  const snapshot = useMemo(
    () => Snapshot.ofSource(sourceId, encoder.encode(text), 'wasm32-unknown-unknown'),
    [text],
  )

  // Emission can fail on a program the backend does not cover; the lab reports that as data
  // rather than breaking the page.
  const emission = useMemo(() => {
    try {
      return {
        _tag: 'Emitted' as const,
        artifact: Effect.runSync(Analysis.codegenWasm(snapshot, { mode })),
      }
    } catch (error) {
      return { _tag: 'Rejected' as const, message: messageOf(error) }
    }
  }, [snapshot, mode])

  const execution = useMemo(
    () => (emission._tag === 'Emitted' ? execute(emission.artifact.bitcode) : undefined),
    [emission],
  )

  // The interpreter runs the same MIR the backend emitted from, so a disagreement is a backend
  // bug and is worth surfacing directly in the lab.
  const interpreted = useMemo(() => Analysis.evaluate(snapshot), [snapshot])
  const layout = Analysis.layoutOf(snapshot)
  const expected = interpreted._tag === 'Completed' ? interpreted.result.value : 'trap'
  const observed =
    execution === undefined
      ? undefined
      : execution._tag === 'Completed'
        ? execution.value
        : execution._tag === 'Trapped'
          ? 'trap'
          : 'failed'
  const agrees = observed !== undefined && observed === expected

  return (
    <div>
      <div className={styles.exampleBar} aria-label="WebAssembly presets">
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
          {mode === 'release' ? 'name section: off' : 'name section: on'}
        </button>
      </div>

      <label className="sr-only" htmlFor="wasm-source">
        Silk source code
      </label>
      <textarea
        id="wasm-source"
        className={styles.editor}
        value={text}
        onChange={(event) => setText(event.target.value)}
        spellCheck={false}
        autoCapitalize="off"
        autoCorrect="off"
      />

      {emission._tag === 'Rejected' ? (
        <section className={styles.diagnosticGroup} aria-labelledby="wasm-rejected">
          <div className={styles.diagnosticHeading}>
            <h3 id="wasm-rejected">Emission rejected</h3>
          </div>
          <pre aria-label="WebAssembly emission error">{emission.message}</pre>
        </section>
      ) : (
        <>
          {layout._tag === 'Available' ? (
            <section className={styles.diagnosticGroup} aria-labelledby="wasm-target-layout">
              <div className={styles.diagnosticHeading}>
                <h3 id="wasm-target-layout">Compiler target and scalar plan</h3>
                <span>{layout.value.target.id}</span>
              </div>
              <ul className={styles.diagnosticList} aria-label="WebAssembly target layout plan">
                {layout.value.entries.map((entry) => (
                  <li key={entry.type}>
                    <div>
                      <code>{entry.type}</code>
                      <span>
                        {entry.size} bytes · align {entry.alignment} · wasm i
                        {entry.representation.bits}
                      </span>
                    </div>
                  </li>
                ))}
              </ul>
            </section>
          ) : null}
          <section className={styles.diagnosticGroup} aria-labelledby="wasm-execution">
            <div className={styles.diagnosticHeading}>
              <h3 id="wasm-execution">Execution</h3>
              {/* The count slot this heading styles is a small circular badge, so the agreement
                  verdict reads as its own row below rather than as a heading annotation. */}
              <span>{agrees ? '=' : '≠'}</span>
            </div>
            <ul className={styles.diagnosticList} aria-label="WebAssembly execution result">
              <li>
                <div>
                  <code>silk_main()</code>
                  <span aria-label="WebAssembly result">
                    {execution === undefined
                      ? 'not run'
                      : execution._tag === 'Completed'
                        ? `returned ${execution.value}`
                        : execution._tag === 'Trapped'
                          ? `trapped — ${execution.message}`
                          : `instantiation failed — ${execution.message}`}
                  </span>
                </div>
              </li>
              <li>
                <div>
                  <code>bootstrap interpreter</code>
                  <span aria-label="Interpreter result">
                    {interpreted._tag === 'Completed'
                      ? `returned ${interpreted.result.value}`
                      : 'trapped'}
                  </span>
                </div>
              </li>
              <li>
                <div>
                  <code>agreement</code>
                  <span aria-label="Backend agreement">
                    {agrees
                      ? 'the backend and the interpreter agree'
                      : 'the backend and the interpreter disagree'}
                  </span>
                </div>
              </li>
            </ul>
          </section>

          <div className={styles.diagnostics}>
            <section className={styles.diagnosticGroup} aria-labelledby="wasm-symbols">
              <div className={styles.diagnosticHeading}>
                <h3 id="wasm-symbols">Symbols</h3>
                <span>{emission.artifact.symbols.length}</span>
              </div>
              <ul className={styles.diagnosticList} aria-label="Backend symbol table">
                {emission.artifact.symbols.map((entry) => (
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

          <section className={styles.diagnosticGroup} aria-labelledby="wasm-text">
            <div className={styles.diagnosticHeading}>
              <h3 id="wasm-text">Emitted WebAssembly text ({mode})</h3>
              <span>{emission.artifact.bitcode.length} binary bytes</span>
            </div>
            <pre aria-label="Emitted WebAssembly text">{emission.artifact.ir}</pre>
          </section>

          <section className={styles.diagnosticGroup} aria-labelledby="wasm-binary">
            <div className={styles.diagnosticHeading}>
              <h3 id="wasm-binary">Emitted binary</h3>
              <span>{emission.artifact.bitcode.length} bytes</span>
            </div>
            <pre aria-label="Emitted WebAssembly binary">{hex(emission.artifact.bitcode)}</pre>
          </section>
        </>
      )}

      <CfgView module={Analysis.loweredMir(snapshot)} source={text} />
    </div>
  )
}
