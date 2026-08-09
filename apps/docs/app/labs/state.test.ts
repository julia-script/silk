import { Atom, AtomRegistry } from 'effect/unstable/reactivity'
import { expect, it } from 'vitest'
import { cursorAtom, evaluationAtom, modulesAtom, rootAtom, snapshotAtom } from './state'

const source = 'pub fn silk_main() -> i32 { return 7 }'

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

it('a batched preset load never rebuilds the snapshot against a missing root', () => {
  const registry = AtomRegistry.make()
  const cancel = registry.subscribe(snapshotAtom, () => {})

  // Without the batch, the subscriber is notified after the first write, while the new module
  // map still carries the old root — and Snapshot.make throws on a root it cannot find.
  Atom.batch(() => {
    registry.set(modulesAtom, { app: source })
    registry.set(rootAtom, 'app')
  })

  expect(registry.get(snapshotAtom).closure.rootModule).toBe('app')
  cancel()
})
