import * as AnalysisFixture from './support/AnalysisFixture.js'
import * as Effect from 'effect/Effect'
import { it } from '@effect/vitest'
import { describe, expect } from 'vitest'
import type { ViewContext, ViewResult } from '../src/InspectorRegistry.js'
import { siblingsOf, viewById, views } from '../src/InspectorRegistry.js'
import * as ToolchainPlan from '../src/ToolchainPlan.js'

const project = Effect.fnUntraced(function* (
  viewId: string,
  source: string,
  target = 'aarch64-apple-darwin',
) {
  const sourceId = 'memory/docs/unified-layout'
  const snapshot = yield* AnalysisFixture.retainingMain(
    sourceId,
    new TextEncoder().encode(source),
    target,
  )
  const root = snapshot.closure.rootModule
  const context: ViewContext = {
    snapshot,
    modules: { [root]: source },
    root,
    mode: 'release',
    profile: 'release',
    filter: '',
    showTrivia: false,
  }
  const view = viewById(viewId)
  expect(view, viewId).toBeDefined()
  if (view === undefined) throw new Error(`missing view ${viewId}`)
  return view.project(context)
})

/** Rows render label and detail in separate columns, so assertions read the pair. */
const text = (result: ViewResult): string =>
  result.rows.map((row) => `${row.label} ${row.detail ?? ''}`).join('\n')

describe('view registry', () => {
  it('resolves every view by its own id', () => {
    for (const view of views) {
      expect(viewById(view.id)?.id, view.id).toBe(view.id)
    }
  })

  it('has one target-aware LLVM emission view', () => {
    expect(views.filter((view) => view.phase === 'backend').map((view) => view.id)).toEqual([
      'backend',
    ])
  })

  it('reports unknown ids as missing', () => {
    expect(viewById('llvm')).toBeUndefined()
    expect(viewById('not-a-view')).toBeUndefined()
  })

  // The syntax inspector was two panels, and the consolidation first ported only the left one.
  // These are the panels that made it a *syntax* lab rather than a token list.
  it('carries every panel the syntax inspector shipped', () => {
    for (const id of ['tokens', 'tree', 'flow', 'tir', 'diagnostics']) {
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
  it.effect('reports struct facts, ABI lanes, and aggregate lowering events', () =>
    Effect.gen(function* () {
      const result = yield* project(
        'struct-values',
        `struct Pair { left: i32 right: i32 }
fn make() -> Pair { return Pair { right: 2, left: 1 } }
pub fn main() -> i32 { let pair = make() return pair.right }`,
      )

      const rendered = text(result)
      expect(rendered).toContain('struct construction')
      // The compiler owns the reordering from written order to canonical order; both must show.
      expect(rendered).toContain('source order right, left')
      expect(rendered).toContain('canonical order left, right')
      expect(rendered).toContain('field projection chain')
      expect(rendered).toContain('compiler-owned calling shapes')
      expect(rendered).toMatch(/i32:#0, i32:#1/)
      expect(rendered).toContain('construct')
      expect(rendered).toContain('project')
      expect(result.meta).toContain('1 lit')
    }),
  )
})

describe('array values view', () => {
  it.effect('links canonical literals, checks, layouts, and lanes', () =>
    Effect.gen(function* () {
      const result = yield* project(
        'array-values',
        `struct Pair { left: i32 right: i32 }
fn choose(values: [Pair; 2], index: usize) -> i32 { return values[index].left }
pub fn main() -> i32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`,
      )

      const rendered = text(result)
      expect(rendered).toContain('canonical array types')
      expect(rendered).toContain('Array<memory/docs/unified-layout.Pair, 2>')
      expect(rendered).toContain('literal elements')
      expect(rendered).toContain('runtime check < 2')
      expect(rendered).toContain('stride 8')
      expect(rendered).toContain('[1].#0')
      expect(result.rows.some((row) => row.span !== undefined)).toBe(true)
    }),
  )
})

describe('target layout view', () => {
  it.effect('reports nominal catalog entries with their sizes', () =>
    Effect.gen(function* () {
      const result = yield* project(
        'layout',
        `struct Inner { value: i32 }
struct Outer { inner: Inner flag: bool }
pub fn main() -> i32 { return 42 }`,
      )

      const rendered = text(result)
      expect(rendered).toContain('nominal catalog')
      expect(rendered).toContain('memory/docs/unified-layout.Outer')
      expect(rendered).toContain('memory/docs/unified-layout.Inner')
      expect(rendered).toContain('reachable runtime layout')
      // The target is a one-line fact, not a card heading.
      expect(result.facts?.map((fact) => fact.text)).toContain('aarch64-apple-darwin')
    }),
  )

  it.effect('marks a layout that could not be computed rather than dropping it', () =>
    Effect.gen(function* () {
      // A self-referential struct has no finite layout; the row has to say so, because a missing
      // row is indistinguishable from a struct that was never declared.
      const result = yield* project(
        'layout',
        `struct Node { next: Node }
pub fn main() -> i32 { return 42 }`,
      )

      const rendered = text(result)
      expect(rendered).toContain('memory/docs/unified-layout.Node')
      expect(rendered).toContain('unavailable')
      expect(result.rows.some((row) => row.tone === 'warning')).toBe(true)
    }),
  )
})

describe('control DAG view', () => {
  it.effect('shows structured loop regions and lexical repeat outcomes', () =>
    Effect.gen(function* () {
      const result = yield* project(
        'mir',
        `pub fn main() -> i32 {
  let mut value = 0
  while value < 2 { value = value + 1 }
  return value
}`,
      )
      const rendered = text(result)
      expect(rendered).toContain('loop0')
      expect(rendered).toContain('condition r')
      expect(rendered).toContain('repeat loop0')
      expect(result.meta).toContain('region')
      expect(result.rows.some((row) => row.span !== undefined)).toBe(true)
    }),
  )

  it.effect('shows ownership fixed points and source-linked backend control conversion', () =>
    Effect.gen(function* () {
      const source = `pub fn main() -> i32 {
  let mut value = 0
  while value < 2 { value = value + 1 }
  return value
}`
      const ownership = yield* project('ownership', source)
      expect(text(ownership)).toContain('mutable')
      expect(text(ownership)).toContain('loop0 fixed point compatible')

      const backend = yield* project('backend', source)
      expect(text(backend)).toContain('control conversion')
      expect(text(backend)).toContain('LlvmBranch')
      expect(
        backend.rows.some(
          (row) => row.key.startsWith('backend-control-') && row.span !== undefined,
        ),
      ).toBe(true)
    }),
  )

  it.effect('coordinates match facts through the existing TIR, ownership, and MIR panes', () =>
    Effect.gen(function* () {
      const source = `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 42 }) }`
      const tir = yield* project('tir', source)
      const ownership = yield* project('ownership', source)
      const mir = yield* project('mir', source)

      expect(text(tir)).toContain('match shared')
      expect(text(tir)).toContain('guarded')
      expect(text(ownership)).toContain('match shared')
      expect(text(ownership)).toContain('provisional guard')
      expect(text(mir)).toContain('decision memory/docs/unified-layout.Left')
      expect(text(mir)).toContain('guard _')
      expect(mir.facts?.map((fact) => fact.text)).toContain('1 structured match')
      expect(mir.rows.some((row) => row.span !== undefined)).toBe(true)
    }),
  )
})

describe('downstream panes state why they are empty', () => {
  // A blank pane and a pane for a program that never got that far look identical, which hides
  // the phase that actually broke. Every absent phase has to name its reason.
  it.effect('says why MIR is unavailable for an unresolved target', () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.retainingMain(
        'memory/docs/unavailable',
        new TextEncoder().encode('pub fn main() -> i32 { return 42 }'),
        'not-a-real-target',
      )
      const view = viewById('mir')
      expect(view).toBeDefined()
      if (view === undefined) return
      const result = view.project({
        snapshot,
        modules: { [snapshot.closure.rootModule]: 'pub fn main() -> i32 { return 42 }' },
        root: snapshot.closure.rootModule,
        mode: 'release',
        profile: 'release',
        filter: '',
        showTrivia: false,
      })

      expect(result.unavailable).toBeDefined()
      expect(result.unavailable).toContain('MIR unavailable')
      expect(result.rows).toHaveLength(0)
    }),
  )
})

describe('diagnostics view', () => {
  it.effect('reports a clean program as clean rather than as empty', () =>
    Effect.gen(function* () {
      const result = yield* project('diagnostics', 'pub fn main() -> i32 { return 42 }')
      expect(text(result)).toContain('no diagnostics')
      expect(result.meta).toBe('clean')
    }),
  )

  it.effect('carries the error count in its meta and tones the row', () =>
    Effect.gen(function* () {
      const result = yield* project(
        'diagnostics',
        `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer( }`,
      )
      expect(result.rows.some((row) => row.tone === 'error')).toBe(true)
      expect(result.meta).toMatch(/err/)
    }),
  )
})

describe('concrete tree view', () => {
  it.effect('renders a token the parser had to insert as a real, amber row', () =>
    Effect.gen(function* () {
      // The tree is lossless, so recovery has to be visible in it — a missing token that is simply
      // absent from the tree would make the recovery invisible exactly where it matters.
      const result = yield* project(
        'tree',
        `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer( }`,
      )
      const missing = result.rows.filter((row) => row.dot === 'missing')
      expect(missing.length).toBeGreaterThan(0)
      expect(missing[0]?.detail).toContain('recovery')
      expect(missing[0]?.tone).toBe('warning')
    }),
  )

  it.effect('hides trivia by default and includes it when asked', () =>
    Effect.gen(function* () {
      const source = 'pub fn main() -> i32 { return 42 }'
      const view = viewById('tree')
      if (view === undefined) throw new Error('missing tree view')
      const snapshot = yield* AnalysisFixture.retainingMain(
        'memory/docs/trivia',
        new TextEncoder().encode(source),
      )
      const base = {
        snapshot,
        modules: { [snapshot.closure.rootModule]: source },
        root: snapshot.closure.rootModule,
        mode: 'release' as const,
        profile: 'release' as const,
        filter: '',
      }

      const without = view.project({ ...base, showTrivia: false })
      const with_ = view.project({ ...base, showTrivia: true })
      expect(with_.rows.length).toBeGreaterThan(without.rows.length)
    }),
  )
})

describe('optimization profile', () => {
  // Inspector clients derive codegen's debug-info mode from the profile instead of carrying a second
  // control. That only stays honest while `-g` profiles map to debug mode: if they diverged, the
  // backend pane would show stripped IR for a build the toolchain plans with debug info.
  it('keeps debug info whenever the profile asks clang for -g', () => {
    expect(ToolchainPlan.codegenModeFor('debug')).toBe('debug')
    expect(ToolchainPlan.codegenModeFor('release-with-debug')).toBe('debug')
    expect(ToolchainPlan.codegenModeFor('release')).toBe('release')
  })
})

describe('toolchain projection', () => {
  it.effect('plans LLVM-to-Wasm finalization for the WebAssembly target', () =>
    Effect.gen(function* () {
      const rendered = text(
        yield* project('toolchain', 'pub fn main() -> i32 { return 42 }', 'wasm32-unknown-unknown'),
      )
      expect(rendered).toContain('--target=wasm32-unknown-unknown')
      expect(rendered).toContain('--export-dynamic')
    }),
  )
})
