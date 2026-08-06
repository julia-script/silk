/**
 * Named saved layouts.
 *
 * "Reset layout" could only ever get you back to one arrangement, so any pane set worth keeping
 * had to be rebuilt by hand. A workspace is that arrangement given a name; the four seeded ones
 * are the pane sets the phases are actually compared in.
 *
 * A workspace stores only *which* views are open and how they are arranged — never the program.
 * Source lives in the URL, so switching workspaces re-frames the program you already have rather
 * than replacing it.
 */

import type { ViewId } from './registry'

/** A pane's slot in the seeded two-row grid. */
export type Slot = 'a1' | 'a2' | 'b1' | 'b2' | 'c1' | 'c2'

export interface Workspace {
  readonly name: string
  /** Ordered by slot: the first three are the top row, the last three the bottom. */
  readonly panes: Readonly<Record<Slot, ViewId>>
}

export const seededWorkspaces: ReadonlyArray<Workspace> = [
  {
    name: 'Syntax',
    panes: { a1: 'source', a2: 'tokens', b1: 'tree', b2: 'diagnostics', c1: 'index', c2: 'closure' },
  },
  {
    name: 'Semantics',
    panes: {
      a1: 'source',
      a2: 'diagnostics',
      b1: 'resolution',
      b2: 'hir',
      c1: 'flow',
      c2: 'ownership',
    },
  },
  {
    name: 'Backend triage',
    panes: { a1: 'source', a2: 'tokens', b1: 'tree', b2: 'hir', c1: 'mir', c2: 'backend' },
  },
  {
    name: 'Everything',
    panes: {
      a1: 'source',
      a2: 'instances',
      b1: 'layout',
      b2: 'evaluation',
      c1: 'toolchain',
      c2: 'diagnostics',
    },
  },
]

export const slotOrder: ReadonlyArray<Slot> = ['a1', 'a2', 'b1', 'b2', 'c1', 'c2']

export const workspaceStorageKey = 'silk-labs-workbench-workspaces'
