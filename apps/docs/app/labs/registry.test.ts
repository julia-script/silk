import { Analysis, ToolchainPlan } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import type { ViewContext } from './registry'
import { viewById, views } from './registry'

const renderLayout = (source: string): string => {
  const sourceId = 'memory/docs/unified-layout'
  const snapshot = Analysis.ofSource(
    sourceId,
    new TextEncoder().encode(source),
    'aarch64-apple-darwin',
  )
  const root = snapshot.closure.rootModule
  const context: ViewContext = {
    snapshot,
    modules: { [root]: source },
    root,
    mode: 'release',
    profile: 'release',
    selectedDiagnostic: undefined,
    onSelectDiagnostic: () => undefined,
    selectedFlowId: undefined,
    onSelectFlow: () => undefined,
    evaluation: undefined,
    onEvaluate: () => undefined,
  }
  const view = viewById('layout')
  expect(view).toBeDefined()
  return view === undefined ? '' : renderToStaticMarkup(view.render(context))
}

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

  it('renders nominal catalog facts in the unified target-layout pane', () => {
    const markup = renderLayout(`struct Inner { value: I32 }
struct Outer { inner: Inner flag: Bool }
pub fn main() -> I32 { return 42 }`)

    expect(markup).toContain('Declaration-wide nominal layout catalog')
    expect(markup).toContain('2 declarations')
    expect(markup).toContain('memory/docs/unified-layout.Outer')
    expect(markup).toContain('Dependencies: memory/docs/unified-layout.Inner')
    expect(markup).toContain(
      'aria-label="Physical fields of memory/docs/unified-layout.Outer"',
    )
    expect(markup).toContain('#0 inner: memory/docs/unified-layout.Inner')
    expect(markup).toContain('offset 0 · padding 0 · size 4 · align 4')
    expect(markup).toContain('tail padding 0')
    expect(markup).toContain('Reachable runtime layout plan')
  })

  it('keeps recursive layout failures and their canonical causes visible', () => {
    const markup = renderLayout(`struct Node { next: Node }
pub fn main() -> I32 { return 42 }`)

    expect(markup).toContain('memory/docs/unified-layout.Node')
    expect(markup).toContain('unavailable · invalid declaration')
    expect(markup).toContain('Dependencies: memory/docs/unified-layout.Node')
    expect(markup).toContain('Diagnostic cause: SEM0020 at memory/docs/unified-layout:')
  })
})

describe('optimization profile', () => {
  // The workbench derives codegen's debug-info mode from the profile instead of carrying a second
  // control. That only stays honest while `-g` profiles map to debug mode: if they diverged, the
  // backend pane would show stripped IR for a build the toolchain plans with debug info.
  it('keeps debug info whenever the profile asks clang for -g', () => {
    expect(ToolchainPlan.codegenModeFor('debug')).toBe('debug')
    expect(ToolchainPlan.codegenModeFor('release-with-debug')).toBe('debug')
    expect(ToolchainPlan.codegenModeFor('release')).toBe('release')
  })
})
