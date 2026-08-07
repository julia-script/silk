'use client'

/**
 * The interpreter lab: the `@silk-effect/wasm` direct interpreter driven as a debugger.
 *
 * One demo module on the left as an instruction listing, the live machine on the right. The
 * session pauses between any two instructions, so every control here is just a synchronous
 * `Interp` call followed by a re-render: step, step over, step out, resume-to-breakpoint.
 * Clicking an instruction row toggles a breakpoint on that exact `Instr` value — the same
 * value identity the interpreter reports back in its paused location.
 */

import type * as Instr from '@silk-effect/wasm/Instr'
import * as Interp from '@silk-effect/wasm/Interp'
import * as Effect from 'effect/Effect'
import { type ReactNode, useEffect, useMemo, useReducer, useRef, useState } from 'react'
import { RowList, type RowModel } from '../row/row'
import { makeDemo } from './demo'
import styles from './interp.module.css'
import {
  frameRows,
  globalRows,
  listingRows,
  localRows,
  memoryRows,
  stackRows,
} from './rows'

interface Run {
  readonly instance: Interp.Instance
  readonly session: Interp.Session
}

function Pane({
  title,
  meta,
  rows,
  empty,
}: {
  readonly title: string
  readonly meta?: string
  readonly rows: ReadonlyArray<RowModel>
  readonly empty: string
}) {
  return (
    <section className={styles.pane}>
      <header className={styles.paneHeader}>
        <span>{title}</span>
        <span className={styles.paneMeta}>{meta ?? ''}</span>
      </header>
      {rows.length === 0 ? (
        <p className={styles.empty}>{empty}</p>
      ) : (
        <RowList rows={rows} label={`${title} rows`} />
      )}
    </section>
  )
}

export function InterpLab() {
  const demo = useMemo(makeDemo, [])
  const [, force] = useReducer((tick: number) => tick + 1, 0)
  const [entryName, setEntryName] = useState(demo.functions[0]?.name ?? 'fib')
  const [argText, setArgText] = useState(`${demo.functions[0]?.params[0]?.sample ?? 8}`)
  const [run, setRun] = useState<Run | undefined>(undefined)
  const [breakpoints] = useState(() => new Set<Instr.Instr>())

  const entry = demo.functions.find((fn) => fn.name === entryName) ?? demo.functions[0]

  const start = () => {
    if (entry === undefined) return
    const instance = demo.instantiate()
    const args = [Number.parseInt(argText, 10) | 0]
    const session = Effect.runSync(Interp.debug(instance, entry.name, args))
    for (const instr of breakpoints) Interp.setBreakpoint(session, instr)
    setRun({ instance, session })
  }

  const withSession = (act: (session: Interp.Session) => void) => () => {
    if (run === undefined) return
    act(run.session)
    force()
  }

  const toggleBreakpoint = (instr: Instr.Instr) => {
    if (breakpoints.has(instr)) {
      breakpoints.delete(instr)
      if (run !== undefined) Interp.clearBreakpoint(run.session, instr)
    } else {
      breakpoints.add(instr)
      if (run !== undefined) Interp.setBreakpoint(run.session, instr)
    }
    force()
  }

  const status = run === undefined ? undefined : Interp.status(run.session)
  const paused = status?._tag === 'Paused'
  const current = status?._tag === 'Paused' ? status.location.instr : undefined

  // Keep the paused instruction in view — the second loop of store_series sits below the fold.
  const listingRef = useRef<HTMLElement>(null)
  useEffect(() => {
    listingRef.current
      ?.querySelector('[data-tone="symbol"]')
      ?.scrollIntoView({ block: 'nearest' })
  }, [current])

  const listing = demo.functions.flatMap((fn) =>
    listingRows(fn, demo.names, { current, breakpoints, onToggle: toggleBreakpoint }),
  )

  let statusLine: ReactNode = 'no session — pick an entry and start'
  let statusTone: 'ok' | 'warn' | 'run' | undefined
  if (run !== undefined && status !== undefined) {
    const steps = `${Interp.stepCount(run.session)} steps`
    if (status._tag === 'Paused') {
      statusTone = 'run'
      statusLine = `paused in ${status.location.funcName ?? '?'} · ${steps}`
    } else if (status._tag === 'Done') {
      statusTone = 'ok'
      statusLine = `done → ${status.results.join(', ')} · ${steps}`
    } else {
      statusTone = 'warn'
      statusLine = `trapped — ${status.message} · ${steps}`
    }
  }

  return (
    <div className={`workbenchRoot ${styles.root}`}>
      <header className={styles.bar}>
        <span className={styles.title}>Interpreter lab</span>
        <span className={styles.subtitle}>@silk-effect/wasm/Interp · direct interpretation</span>
        <span className={styles.spacer} />
        <label className={styles.field}>
          entry
          <select
            className={styles.select}
            value={entry?.name}
            onChange={(event) => {
              const next = demo.functions.find((fn) => fn.name === event.target.value)
              setEntryName(event.target.value)
              setArgText(`${next?.params[0]?.sample ?? 0}`)
            }}
          >
            {demo.functions.map((fn) => (
              <option key={fn.name} value={fn.name}>
                {fn.name}
              </option>
            ))}
          </select>
        </label>
        <label className={styles.field}>
          {entry?.params[0]?.name ?? 'n'}
          <input
            className={styles.input}
            type="number"
            value={argText}
            onChange={(event) => setArgText(event.target.value)}
          />
        </label>
        <button className={styles.button} type="button" onClick={start}>
          {run === undefined ? 'start' : 'restart'}
        </button>
        <span className={styles.divider} />
        <button
          className={styles.button}
          type="button"
          disabled={!paused}
          onClick={withSession((session) => Interp.step(session))}
        >
          step
        </button>
        <button
          className={styles.button}
          type="button"
          disabled={!paused}
          onClick={withSession((session) => Interp.stepOver(session))}
        >
          over
        </button>
        <button
          className={styles.button}
          type="button"
          disabled={!paused}
          onClick={withSession((session) => Interp.stepOut(session))}
        >
          out
        </button>
        <button
          className={styles.button}
          type="button"
          disabled={!paused}
          onClick={withSession((session) => Interp.resume(session))}
        >
          resume
        </button>
      </header>
      <div className={styles.status} data-tone={statusTone}>
        {statusLine}
        <span className={styles.spacer} />
        <span className={styles.hint}>click an instruction to toggle a breakpoint</span>
      </div>
      <div className={styles.main}>
        <section className={styles.listing} ref={listingRef}>
          <RowList rows={listing} label="module listing" />
        </section>
        <aside className={styles.inspectors}>
          <Pane
            title="Call stack"
            meta={run === undefined ? undefined : `${Interp.callStack(run.session).length}`}
            rows={run === undefined ? [] : frameRows(Interp.callStack(run.session))}
            empty="no frames"
          />
          <Pane
            title="Locals"
            rows={run === undefined ? [] : localRows(Interp.locals(run.session))}
            empty="no frame"
          />
          <Pane
            title="Value stack"
            rows={run === undefined ? [] : stackRows(Interp.valueStack(run.session))}
            empty="empty"
          />
          <Pane
            title="Globals"
            rows={run === undefined ? [] : globalRows(Interp.globals(run.instance))}
            empty="no instance"
          />
          <Pane
            title="Memory · first 64 B"
            meta={run === undefined ? undefined : `${Interp.memoryPageCount(run.instance)} page`}
            rows={run === undefined ? [] : memoryRows(Interp.readMemory(run.instance, 0, 0, 64))}
            empty="no instance"
          />
        </aside>
      </div>
    </div>
  )
}
