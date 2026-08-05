/**
 * Emission-time index resolution.
 *
 * Public handles carry entry indices in declaration order. The binary and text formats number
 * each index space with imported entities first, so emission computes one mapping per space:
 * imported entries keep declaration order and claim the low indices, then defined entries follow
 * in declaration order.
 *
 * @internal
 */
import type * as ModuleState from './ModuleState.js'

export interface Spaces {
  /** Entry index to emitted index, per space. */
  readonly funcs: ReadonlyArray<number>
  readonly tables: ReadonlyArray<number>
  readonly memories: ReadonlyArray<number>
  readonly globals: ReadonlyArray<number>
  readonly tags: ReadonlyArray<number>
  /** Emitted order of entries: imported first, then defined, both in declaration order. */
  readonly funcOrder: ReadonlyArray<number>
  readonly tableOrder: ReadonlyArray<number>
  readonly memoryOrder: ReadonlyArray<number>
  readonly globalOrder: ReadonlyArray<number>
  readonly tagOrder: ReadonlyArray<number>
}

const resolveKind = (
  entries: ReadonlyArray<{ readonly importSource: ModuleState.ImportSource | undefined }>,
): { readonly indices: ReadonlyArray<number>; readonly order: ReadonlyArray<number> } => {
  const imported: Array<number> = []
  const defined: Array<number> = []
  for (const [entryIndex, entry] of entries.entries()) {
    ;(entry.importSource === undefined ? defined : imported).push(entryIndex)
  }
  const order = [...imported, ...defined]
  const indices = new Array<number>(entries.length)
  for (const [emitted, entryIndex] of order.entries()) {
    indices[entryIndex] = emitted
  }
  return { indices, order }
}

/** @internal */
export const resolve = (snapshot: ModuleState.Snapshot): Spaces => {
  const funcs = resolveKind(snapshot.funcs)
  const tables = resolveKind(snapshot.tables)
  const memories = resolveKind(snapshot.memories)
  const globals = resolveKind(snapshot.globals)
  const tags = resolveKind(snapshot.tags)
  return {
    funcs: funcs.indices,
    tables: tables.indices,
    memories: memories.indices,
    globals: globals.indices,
    tags: tags.indices,
    funcOrder: funcs.order,
    tableOrder: tables.order,
    memoryOrder: memories.order,
    globalOrder: globals.order,
    tagOrder: tags.order,
  }
}
