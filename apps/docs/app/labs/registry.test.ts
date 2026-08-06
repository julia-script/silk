import { Analysis, ToolchainPlan } from '@silk-effect/compiler'
import { describe, expect, it } from 'vitest'
import type { ViewContext, ViewResult } from './registry'
import { siblingsOf, viewById, views } from './registry'

const project = (viewId: string, source: string): ViewResult => {
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
    cursor: undefined,
    onSelectSpan: () => undefined,
    evaluation: undefined,
    onEvaluate: () => undefined,
    filter: '',
    showTrivia: false,
  }
  const view = viewById(viewId)
  expect(view, viewId).toBeDefined()
  if (view === undefined) throw new Error(`missing view ${viewId}`)
  return view.project(context)
}

/** Rows render label and detail in separate columns, so assertions read the pair. */
const text = (result: ViewResult): string =>
  result.rows.map((row) => `${row.label} ${row.detail ?? ''}`).join('\n')

/**
 * The struct-values view wants the whole pipeline: an evaluated wasm run, so the aggregate
 * Construct/Project events are in the trace alongside the elaboration facts.
 */
const projectStructValues = (source: string): ViewResult => {
  const sourceId = 'memory/docs/unified-struct-values'
  const snapshot = Analysis.ofSource(
    sourceId,
    new TextEncoder().encode(source),
    'wasm32-unknown-unknown',
  )
  const evaluation = Analysis.evaluate(snapshot)
  expect(evaluation._tag).toBe('Completed')
  const view = viewById('struct-values')
  expect(view).toBeDefined()
  if (view === undefined) throw new Error('missing struct-values view')
  return view.project({
    snapshot,
    modules: { [sourceId]: source },
    root: sourceId,
    mode: 'release',
    profile: 'release',
    cursor: undefined,
    onSelectSpan: () => undefined,
    evaluation,
    onEvaluate: () => undefined,
    filter: '',
    showTrivia: false,
  })
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

  it('offers sibling phases from the same group, and never itself', () => {
    const tree = viewById('tree')
    expect(tree).toBeDefined()
    if (tree === undefined) return
    const siblings = siblingsOf(tree)
    expect(siblings.map((view) => view.id)).not.toContain('tree')
    for (const sibling of siblings) expect(sibling.group).toBe(tree.group)
  })
})

describe('struct values view', () => {
  it('reports struct facts, ABI lanes, and aggregate evaluation events', () => {
    const result = projectStructValues(`struct Pair { left: I32 right: I32 }
fn make() -> Pair { return Pair { right: 2, left: 1 } }
pub fn main() -> I32 { let pair = make() return pair.right }`)

    const rendered = text(result)
    expect(rendered).toContain('struct construction')
    // The compiler owns the reordering from written order to canonical order; both must show.
    expect(rendered).toContain('source order right, left')
    expect(rendered).toContain('canonical order left, right')
    expect(rendered).toContain('field projection chain')
    expect(rendered).toContain('compiler-owned calling shapes')
    expect(rendered).toMatch(/I32:#0, I32:#1/)
    expect(rendered).toContain('construct')
    expect(rendered).toContain('project')
    expect(result.meta).toContain('1 lit')
  })
})

describe('target layout view', () => {
  it('reports nominal catalog entries with their sizes', () => {
    const result = project(
      'layout',
      `struct Inner { value: I32 }
struct Outer { inner: Inner flag: Bool }
pub fn main() -> I32 { return 42 }`,
    )

    const rendered = text(result)
    expect(rendered).toContain('nominal catalog')
    expect(rendered).toContain('memory/docs/unified-layout.Outer')
    expect(rendered).toContain('memory/docs/unified-layout.Inner')
    expect(rendered).toContain('reachable runtime layout')
    // The target is a one-line fact, not a card heading.
    expect(result.facts?.map((fact) => fact.text)).toContain('aarch64-apple-darwin')
  })

  it('marks a layout that could not be computed rather than dropping it', () => {
    // A self-referential struct has no finite layout; the row has to say so, because a missing
    // row is indistinguishable from a struct that was never declared.
    const result = project(
      'layout',
      `struct Node { next: Node }
pub fn main() -> I32 { return 42 }`,
    )

    const rendered = text(result)
    expect(rendered).toContain('memory/docs/unified-layout.Node')
    expect(rendered).toContain('unavailable')
    expect(result.rows.some((row) => row.tone === 'warning')).toBe(true)
  })
})

describe('downstream panes state why they are empty', () => {
  // A blank pane and a pane for a program that never got that far look identical, which hides
  // the phase that actually broke. Every absent phase has to name its reason.
  it('says why MIR is unavailable for an unresolved target', () => {
    const snapshot = Analysis.ofSource(
      'memory/docs/unavailable',
      new TextEncoder().encode('pub fn main() -> I32 { return 42 }'),
      'not-a-real-target',
    )
    const view = viewById('mir')
    expect(view).toBeDefined()
    if (view === undefined) return
    const result = view.project({
      snapshot,
      modules: { [snapshot.closure.rootModule]: 'pub fn main() -> I32 { return 42 }' },
      root: snapshot.closure.rootModule,
      mode: 'release',
      profile: 'release',
      cursor: undefined,
      onSelectSpan: () => undefined,
      evaluation: undefined,
      onEvaluate: () => undefined,
      filter: '',
      showTrivia: false,
    })

    expect(result.unavailable).toBeDefined()
    expect(result.unavailable).toContain('MIR unavailable')
    expect(result.rows).toHaveLength(0)
  })
})

describe('diagnostics view', () => {
  it('reports a clean program as clean rather than as empty', () => {
    const result = project('diagnostics', 'pub fn main() -> I32 { return 42 }')
    expect(text(result)).toContain('no diagnostics')
    expect(result.meta).toBe('clean')
  })

  it('carries the error count in its meta and tones the row', () => {
    const result = project(
      'diagnostics',
      `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer( }`,
    )
    expect(result.rows.some((row) => row.tone === 'error')).toBe(true)
    expect(result.meta).toMatch(/err/)
  })
})

describe('concrete tree view', () => {
  it('renders a token the parser had to insert as a real, amber row', () => {
    // The tree is lossless, so recovery has to be visible in it — a missing token that is simply
    // absent from the tree would make the recovery invisible exactly where it matters.
    const result = project(
      'tree',
      `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer( }`,
    )
    const missing = result.rows.filter((row) => row.dot === 'missing')
    expect(missing.length).toBeGreaterThan(0)
    expect(missing[0]?.detail).toContain('recovery')
    expect(missing[0]?.tone).toBe('warning')
  })

  it('hides trivia by default and includes it when asked', () => {
    const source = 'pub fn main() -> I32 { return 42 }'
    const view = viewById('tree')
    if (view === undefined) throw new Error('missing tree view')
    const snapshot = Analysis.ofSource('memory/docs/trivia', new TextEncoder().encode(source))
    const base = {
      snapshot,
      modules: { [snapshot.closure.rootModule]: source },
      root: snapshot.closure.rootModule,
      mode: 'release' as const,
      profile: 'release' as const,
      cursor: undefined,
      onSelectSpan: () => undefined,
      evaluation: undefined,
      onEvaluate: () => undefined,
      filter: '',
    }

    const without = view.project({ ...base, showTrivia: false })
    const with_ = view.project({ ...base, showTrivia: true })
    expect(with_.rows.length).toBeGreaterThan(without.rows.length)
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
