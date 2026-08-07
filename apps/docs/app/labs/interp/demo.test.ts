import * as Interp from '@silk-effect/wasm/Interp'
import * as Effect from 'effect/Effect'
import { describe, expect, it } from 'vitest'
import { makeDemo } from './demo'
import { listingRows } from './rows'

describe('interp lab demo module', () => {
  const demo = makeDemo()

  it('computes the expected results', () => {
    const instance = demo.instantiate()
    expect(Effect.runSync(Interp.invoke(instance, 'fib', [8]))).toEqual([21])
    expect(Effect.runSync(Interp.invoke(instance, 'sum_to', [10]))).toEqual([55])
    expect(Effect.runSync(Interp.invoke(instance, 'store_series', [12]))).toEqual([78])
  })

  it('counts fib entries in the mutable global', () => {
    const instance = demo.instantiate()
    Effect.runSync(Interp.invoke(instance, 'fib', [5]))
    // fib(5) enters fib 15 times.
    expect(Interp.globals(instance)[0]?.value).toBe(15)
  })

  it('pauses on a breakpoint set from the listing', () => {
    const instance = demo.instantiate()
    const fib = demo.functions.find((fn) => fn.name === 'fib')
    expect(fib).toBeDefined()
    if (fib === undefined) return
    const session = Effect.runSync(Interp.debug(instance, 'fib', [6]))
    // The if-condition read: executed once per fib entry.
    const target = fib.body[4]
    expect(target).toBeDefined()
    if (target === undefined) return
    Interp.setBreakpoint(session, target)
    const status = Interp.resume(session)
    expect(status._tag).toBe('Paused')
    if (status._tag === 'Paused') expect(status.location.instr).toBe(target)
    Interp.clearBreakpoint(session, target)
    expect(Interp.resume(session)).toEqual({ _tag: 'Done', results: [8] })
  })

  it('projects listings with a current marker and breakpoint dots', () => {
    const fib = demo.functions.find((fn) => fn.name === 'fib')
    if (fib === undefined) throw new Error('missing fib')
    const current = fib.body[0]
    const breakpoint = fib.body[4]
    if (current === undefined || breakpoint === undefined) throw new Error('missing instrs')
    const rows = listingRows(fib, demo.names, {
      current,
      breakpoints: new Set([breakpoint]),
      onToggle: () => {},
    })
    expect(rows[0]?.label).toBe('func $fib')
    expect(rows.filter((row) => row.caret === '▸')).toHaveLength(1)
    expect(rows.filter((row) => row.dot === 'error')).toHaveLength(1)
    expect(rows.some((row) => row.label === 'call $fib')).toBe(true)
    expect(rows.some((row) => row.label === 'global.get $fib_calls')).toBe(true)
  })
})
