'use client'

/**
 * Phase views that had no standalone component, because their old lab fused the view into the
 * wrapper that owned the editor. Each one here takes the workbench's shared snapshot and renders
 * only its phase — the five labs that already exported a `*View` are reused as-is.
 */

import { Analysis, ToolchainPlan } from '@silk-effect/compiler'
import type { Backend } from '@silk-effect/compiler'
import * as Effect from 'effect/Effect'
import { useMemo } from 'react'
import styles from './syntax-inspector/syntax-inspector.module.css'

const clang = '/usr/bin/clang'

const messageOf = (error: unknown): string =>
  error instanceof Error ? error.message : String(error)

const commandText = (planned: ToolchainPlan.PlannedCommand): string =>
  [planned.command, ...planned.arguments].join(' ')

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

/** The outcome of instantiating the emitted module and calling its entry export. */
type Execution =
  | { readonly _tag: 'Completed'; readonly value: number }
  | { readonly _tag: 'Trapped'; readonly message: string }
  | { readonly _tag: 'Failed'; readonly message: string }

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

/**
 * Emission is an Effect that fails on a target the backend cannot serve — a native backend asked
 * for a wasm target, say. Running it here turns that into data the pane can render, instead of an
 * exception that takes the pane down with it.
 */
const emit = (
  run: () => Effect.Effect<Backend.Artifact, unknown>,
):
  | { readonly _tag: 'Emitted'; readonly artifact: Backend.Artifact }
  | { readonly _tag: 'Rejected'; readonly message: string } => {
  try {
    return { _tag: 'Emitted', artifact: Effect.runSync(run()) }
  } catch (error) {
    return { _tag: 'Rejected', message: messageOf(error) }
  }
}

/** The target and its scalar plan, which is what makes an emission reproducible. */
export function LayoutSummary({
  snapshot,
  id,
  label,
}: {
  readonly snapshot: Analysis.Snapshot
  readonly id: string
  readonly label: string
}) {
  const layout = Analysis.layoutOf(snapshot)
  if (layout._tag !== 'Available') {
    return (
      <section className={styles.diagnosticGroup} aria-labelledby={id}>
        <div className={styles.diagnosticHeading}>
          <h3 id={id}>Compiler target and scalar plan</h3>
        </div>
        <p className={styles.emptyState}>{layout.error.message}</p>
      </section>
    )
  }

  return (
    <section className={styles.diagnosticGroup} aria-labelledby={id}>
      <div className={styles.diagnosticHeading}>
        <h3 id={id}>Compiler target and scalar plan</h3>
        <span>{layout.value.target.id}</span>
      </div>
      <ul className={styles.diagnosticList} aria-label={label}>
        {layout.value.entries.map((entry) => (
          <li key={entry.type}>
            <div>
              <code>{entry.type}</code>
              <span>
                {entry.size} bytes · align {entry.alignment} · i{entry.representation.bits}
              </span>
            </div>
          </li>
        ))}
      </ul>
    </section>
  )
}

export function LlvmView({
  snapshot,
  mode,
}: {
  readonly snapshot: Analysis.Snapshot
  readonly mode: 'release' | 'debug'
}) {
  const emission = useMemo(
    () => emit(() => Analysis.codegen(snapshot, { mode })),
    [snapshot, mode],
  )

  if (emission._tag === 'Rejected') {
    return (
      <div>
        <LayoutSummary snapshot={snapshot} id="llvm-target-layout" label="LLVM target layout plan" />
        <section className={styles.diagnosticGroup} aria-labelledby="llvm-rejected">
          <div className={styles.diagnosticHeading}>
            <h3 id="llvm-rejected">Emission rejected</h3>
          </div>
          <pre aria-label="LLVM emission error">{emission.message}</pre>
        </section>
      </div>
    )
  }

  const artifact = emission.artifact

  return (
    <div>
      <div className={styles.diagnostics}>
        <LayoutSummary snapshot={snapshot} id="llvm-target-layout" label="LLVM target layout plan" />
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
        <pre aria-label="Emitted LLVM IR">{artifact.ir}</pre>
      </section>
    </div>
  )
}

export function WasmView({
  snapshot,
  mode,
}: {
  readonly snapshot: Analysis.Snapshot
  readonly mode: 'release' | 'debug'
}) {
  // Emission can fail on a program the backend does not cover, and on a snapshot whose target is
  // not one this backend serves; the panel reports either as data rather than breaking the page.
  const emission = useMemo(
    () => emit(() => Analysis.codegenWasm(snapshot, { mode })),
    [snapshot, mode],
  )

  const execution = useMemo(
    () => (emission._tag === 'Emitted' ? execute(emission.artifact.bitcode) : undefined),
    [emission],
  )

  // The interpreter runs the same MIR the backend emitted from, so a disagreement is a backend
  // bug and is worth surfacing directly. `evaluate` throws when MIR is unavailable — an
  // unresolved target, say — so availability is checked rather than assumed.
  const interpreted = useMemo(
    () => (Analysis.mirOf(snapshot)._tag === 'Available' ? Analysis.evaluate(snapshot) : undefined),
    [snapshot],
  )
  const expected =
    interpreted === undefined
      ? undefined
      : interpreted._tag === 'Completed'
        ? interpreted.result.value
        : 'trap'
  const observed =
    execution === undefined
      ? undefined
      : execution._tag === 'Completed'
        ? execution.value
        : execution._tag === 'Trapped'
          ? 'trap'
          : 'failed'
  const agrees = observed !== undefined && observed === expected

  if (emission._tag === 'Rejected') {
    return (
      <div>
        <LayoutSummary
          snapshot={snapshot}
          id="wasm-target-layout"
          label="WebAssembly target layout plan"
        />
        <section className={styles.diagnosticGroup} aria-labelledby="wasm-rejected">
          <div className={styles.diagnosticHeading}>
            <h3 id="wasm-rejected">Emission rejected</h3>
          </div>
          <pre aria-label="WebAssembly emission error">{emission.message}</pre>
        </section>
      </div>
    )
  }

  return (
    <div>
      <LayoutSummary
        snapshot={snapshot}
        id="wasm-target-layout"
        label="WebAssembly target layout plan"
      />

      <section className={styles.diagnosticGroup} aria-labelledby="wasm-execution">
        <div className={styles.diagnosticHeading}>
          <h3 id="wasm-execution">Execution</h3>
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
                {interpreted === undefined
                  ? 'not run — MIR unavailable'
                  : interpreted._tag === 'Completed'
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
    </div>
  )
}

export function ToolchainView({
  snapshot,
  profile,
}: {
  readonly snapshot: Analysis.Snapshot
  readonly profile: ToolchainPlan.OptimizationProfile
}) {
  const emission = useMemo(
    () => emit(() => Analysis.codegen(snapshot, { mode: ToolchainPlan.codegenModeFor(profile) })),
    [snapshot, profile],
  )
  const selection = Analysis.targetOf(snapshot)

  // Every planned command is issued *for* a target, so there is nothing to plan until one
  // resolves. The native toolchain also cannot serve a WebAssembly target — saying so beats
  // rendering a clang line that would never be run.
  if (selection._tag !== 'Resolved') {
    return (
      <section className={styles.diagnosticGroup} aria-labelledby="toolchain-unavailable">
        <div className={styles.diagnosticHeading}>
          <h3 id="toolchain-unavailable">Toolchain unavailable</h3>
        </div>
        <p className={styles.emptyState}>{selection.error.message}</p>
      </section>
    )
  }

  const target = selection.target

  if (target.kind !== 'Native') {
    return (
      <section className={styles.diagnosticGroup} aria-labelledby="toolchain-non-native">
        <div className={styles.diagnosticHeading}>
          <h3 id="toolchain-non-native">Toolchain unavailable</h3>
          <span>{target.id}</span>
        </div>
        <p className={styles.emptyState}>
          The native toolchain orchestrates a link driver, which the {target.id} target has no use
          for. Pick a native target to see its planned commands.
        </p>
      </section>
    )
  }

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
      <div className={styles.diagnostics}>
        <section className={styles.diagnosticGroup} aria-labelledby="toolchain-commands">
          <div className={styles.diagnosticHeading}>
            <h3 id="toolchain-commands">Planned commands · {profile}</h3>
            <span>
              {emission._tag === 'Emitted'
                ? `${emission.artifact.bitcode.length} bitcode bytes`
                : 'emission rejected'}
            </span>
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
