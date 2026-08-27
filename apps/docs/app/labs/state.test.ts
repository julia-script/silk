import { assert, it } from '@effect/vitest'
import * as Analysis from '@silklang/compiler/Analysis'
import * as Result from 'effect/Result'
import * as Schema from 'effect/Schema'
import { Atom, AtomRegistry } from 'effect/unstable/reactivity'
import { afterEach, vi } from 'vitest'
import {
  cursorAtom,
  evaluationAtom,
  modulesAtom,
  rootAtom,
  snapshotAtom,
  sourceUpdateDebounceMs,
} from './state'
import { seededWorkspaces, workspacesSchema } from './workspaces'

const source = 'pub fn silk_main() -> i32 { return 7 }'

afterEach(() => vi.useRealTimers())

it('decodes only current workspace shapes and view ids', () => {
  const current = seededWorkspaces.at(0)
  assert.isDefined(current)
  if (current === undefined) return
  const decode = Schema.decodeUnknownResult(workspacesSchema)

  assert.isTrue(Result.isSuccess(decode([current])))
  assert.isTrue(
    Result.isFailure(
      decode([{ ...current, panes: { ...current.panes, a1: 'llvm' } }]),
    ),
  )
  assert.isTrue(
    Result.isFailure(
      decode([{ ...current, panes: { ...current.panes, retired: 'source' } }]),
    ),
  )
})

it('rebuilds the shared snapshot once after a burst of source edits settles', () => {
  vi.useFakeTimers()
  const registry = AtomRegistry.make()
  const cancel = registry.subscribe(snapshotAtom, () => {})
  const root = registry.get(rootAtom)
  const before = registry.get(snapshotAtom)

  registry.set(modulesAtom, { [root]: `${source} ` })
  assert.strictEqual(registry.get(snapshotAtom), before)
  registry.set(modulesAtom, { [root]: `${source}  ` })
  assert.strictEqual(registry.get(snapshotAtom), before)
  registry.set(modulesAtom, { [root]: `${source}   ` })
  assert.strictEqual(registry.get(snapshotAtom), before)

  vi.advanceTimersByTime(sourceUpdateDebounceMs - 1)
  assert.strictEqual(registry.get(snapshotAtom), before)
  vi.advanceTimersByTime(1)
  assert.notStrictEqual(registry.get(snapshotAtom), before)
  assert.strictEqual(
    Analysis.sources(registry.get(snapshotAtom)).get(root)?.bytes.length,
    source.length + 3,
  )
  cancel()
})

it('a source edit resets the span cursor and evaluation by construction', () => {
  const registry = AtomRegistry.make()
  // A default atom without a subscriber can be collected between set and get.
  const cancelCursor = registry.subscribe(cursorAtom, () => {})
  const cancelEvaluation = registry.subscribe(evaluationAtom, () => {})
  // Build the nodes the way useAtomValue does: the read is what registers the snapshot
  // dependency, and a value set before the first read would have no dependency to reset it.
  assert.isUndefined(registry.get(cursorAtom))
  assert.isUndefined(registry.get(evaluationAtom))

  registry.set(cursorAtom, { module: 'main', start: 0, end: 3 })
  assert.deepEqual(registry.get(cursorAtom), { module: 'main', start: 0, end: 3 })

  registry.set(modulesAtom, { [registry.get(rootAtom)]: source })
  assert.isUndefined(registry.get(cursorAtom))
  assert.isUndefined(registry.get(evaluationAtom))

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

  assert.strictEqual(registry.get(snapshotAtom), before)
  vi.advanceTimersByTime(sourceUpdateDebounceMs)
  assert.strictEqual(registry.get(snapshotAtom).closure.rootModule, 'app')
  cancel()
})
