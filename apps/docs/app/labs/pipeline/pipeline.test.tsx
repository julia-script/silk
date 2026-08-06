import { Analysis } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { PipelineLab, pipelineRows } from './pipeline'

describe('PipelineLab', () => {
  it('lists every phase in order with counts, no diagnostics, and lab links', () => {
    const markup = renderToStaticMarkup(<PipelineLab />)

    expect(markup).toContain('aria-label="Pipeline phases"')
    const order = [
      'syntax (lex + parse)',
      'module closure',
      'declaration index',
      'elaboration (HIR)',
      'ownership + cleanup plan',
      'instance discovery',
      'MIR lowering',
      'backend (LLVM bitcode)',
      'native toolchain (planned)',
    ]
    let cursor = -1
    for (const phase of order) {
      const index = markup.indexOf(phase)
      expect(index, phase).toBeGreaterThan(cursor)
      cursor = index
    }
    expect(markup).toContain('no diagnostics')
    expect(markup).toContain('href="/docs/labs/mir-cfg"')
    expect(markup).toContain('href="/docs/labs/toolchain"')
    expect(markup).toContain('2 + 3 * 4 |&gt; I32.add(1)')
    expect(markup).toMatch(/snapshot built in \d+(\.\d+)? ms/)
  })
})

describe('pipelineRows', () => {
  it('shows per-phase diagnostic counts and explicit recovery states for damaged programs', () => {
    const snapshot = Analysis.ofSource(
      'memory/pipeline-test',
      new TextEncoder().encode('@ pub fn main( -> Mystery { return 42 }'),
    )
    const rows = pipelineRows(snapshot)

    const syntax = rows.find((row) => row.phase.startsWith('syntax'))
    const hir = rows.find((row) => row.phase.startsWith('elaboration'))
    const discovery = rows.find((row) => row.phase.startsWith('instance'))
    expect(syntax?.diagnostics).toBeGreaterThan(0)
    expect(hir?.diagnostics).toBeGreaterThan(0)
    expect(discovery?.outputs).toContain('entry unavailable')
  })
})
