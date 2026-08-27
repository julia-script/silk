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

import { viewIds } from '@silklang/compiler/InspectorRegistry'
import * as Schema from 'effect/Schema'

/** A pane's slot in the seeded two-row grid. */
export type Slot = 'a1' | 'a2' | 'b1' | 'b2' | 'c1' | 'c2'

const viewIdSchema = Schema.Literals(viewIds)

/** The exact persisted shape for one current workspace. */
export const workspaceSchema = Schema.Struct({
  name: Schema.String,
  panes: Schema.Struct({
    a1: viewIdSchema,
    a2: viewIdSchema,
    b1: viewIdSchema,
    b2: viewIdSchema,
    c1: viewIdSchema,
    c2: viewIdSchema,
  }),
}).annotate({ parseOptions: { onExcessProperty: 'error' } })

export const workspacesSchema = Schema.Array(workspaceSchema).annotate({
  parseOptions: { onExcessProperty: 'error' },
})

export type Workspace = typeof workspaceSchema.Type

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
    name: 'Arrays',
    panes: {
      a1: 'source',
      a2: 'diagnostics',
      b1: 'array-values',
      b2: 'ownership',
      c1: 'mir',
      c2: 'evaluation',
    },
  },
  {
    name: 'Generics',
    panes: {
      a1: 'source',
      a2: 'diagnostics',
      b1: 'hir',
      b2: 'instances',
      c1: 'layout',
      c2: 'mir',
    },
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

export const workspaceStorageKey = 'silk-labs-workspaces'
