import { describe, expect, it } from 'vitest'
import { viewById, views } from './registry'

describe('view registry', () => {
  it('resolves every view by its own id', () => {
    for (const view of views) {
      expect(viewById(view.id)?.id, view.id).toBe(view.id)
    }
  })

  it('has one backend view rather than one per backend', () => {
    // The target picks the backend, so a pane per backend would be a choice the user cannot get
    // right — picking the one the target does not serve just yields a rejection.
    expect(views.filter((view) => view.phase === 'backend').map((view) => view.id)).toEqual([
      'backend',
    ])
  })

  // Layouts live in URLs and in localStorage, so a link shared before the merge still names the
  // retired ids. Resolving them keeps those links working instead of showing an unknown view.
  it('still resolves the retired per-backend ids', () => {
    expect(viewById('llvm')?.id).toBe('backend')
    expect(viewById('wasm')?.id).toBe('backend')
  })

  it('reports a genuinely unknown id as missing', () => {
    expect(viewById('not-a-view')).toBeUndefined()
  })

  // The syntax inspector was two panels, and the consolidation first ported only the left one.
  // These are the panels that made it a *syntax* lab rather than a token list.
  it('carries every panel the syntax inspector shipped', () => {
    for (const id of ['tokens', 'tree', 'flow', 'evaluation', 'hir', 'diagnostics']) {
      expect(viewById(id)?.id, id).toBe(id)
    }
  })
})
