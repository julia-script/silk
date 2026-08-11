import * as Analysis from '@silk-effect/compiler/Analysis'
import { Atom, AtomRegistry } from 'effect/unstable/reactivity'
import { afterEach, expect, it, vi } from 'vitest'
import {
  cursorAtom,
  evaluationAtom,
  modulesAtom,
  rootAtom,
  snapshotAtom,
  sourceUpdateDebounceMs,
} from './state'

const source = 'pub fn silk_main() -> i32 { return 7 }'

afterEach(() => vi.useRealTimers())

it('rebuilds the shared snapshot once after a burst of source edits settles', () => {
  vi.useFakeTimers()
  const registry = AtomRegistry.make()
  const cancel = registry.subscribe(snapshotAtom, () => {})
  const root = registry.get(rootAtom)
  const before = registry.get(snapshotAtom)

  registry.set(modulesAtom, { [root]: `${source} ` })
  expect(registry.get(snapshotAtom) === before).toBe(true)
  registry.set(modulesAtom, { [root]: `${source}  ` })
  expect(registry.get(snapshotAtom) === before).toBe(true)
  registry.set(modulesAtom, { [root]: `${source}   ` })
  expect(registry.get(snapshotAtom) === before).toBe(true)

  vi.advanceTimersByTime(sourceUpdateDebounceMs - 1)
  expect(registry.get(snapshotAtom) === before).toBe(true)
  vi.advanceTimersByTime(1)
  expect(registry.get(snapshotAtom) === before).toBe(false)
  expect(Analysis.sources(registry.get(snapshotAtom)).get(root)?.bytes.length).toBe(source.length + 3)
  cancel()
})

it('a source edit resets the span cursor and evaluation by construction', () => {
  const registry = AtomRegistry.make()
  // A default atom without a subscriber can be collected between set and get.
  const cancelCursor = registry.subscribe(cursorAtom, () => {})
  const cancelEvaluation = registry.subscribe(evaluationAtom, () => {})
  // Build the nodes the way useAtomValue does: the read is what registers the snapshot
  // dependency, and a value set before the first read would have no dependency to reset it.
  expect(registry.get(cursorAtom)).toBeUndefined()
  expect(registry.get(evaluationAtom)).toBeUndefined()

  registry.set(cursorAtom, { start: 0, end: 3 })
  expect(registry.get(cursorAtom)).toEqual({ start: 0, end: 3 })

  registry.set(modulesAtom, { [registry.get(rootAtom)]: source })
  expect(registry.get(cursorAtom)).toBeUndefined()
  expect(registry.get(evaluationAtom)).toBeUndefined()

  cancelCursor()
  cancelEvaluation()
})

it('keeps a preset root and its sources paired while the new snapshot settles', () => {
  vi.useFakeTimers()
  const registry = AtomRegistry.make()
  const cancel = registry.subscribe(snapshotAtom, () => {})
  const before = registry.get(snapshotAtom)

  Atom.batch(() => {
    registry.set(modulesAtom, { app: source })
    registry.set(rootAtom, 'app')
  })

  expect(registry.get(snapshotAtom)).toBe(before)
  vi.advanceTimersByTime(sourceUpdateDebounceMs)
  expect(registry.get(snapshotAtom).closure.rootModule).toBe('app')
  cancel()
})
