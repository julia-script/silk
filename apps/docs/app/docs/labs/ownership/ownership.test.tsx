import { Analysis } from '@silk-effect/compiler'
import type { Ownership } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { OwnershipLab, OwnershipView } from './ownership'

const encoder = new TextEncoder()

const check = (text: string): Ownership.ModuleOwnership | undefined =>
  Analysis.ownershipOf(
    Analysis.ofSource('memory://ownership-test.silk', encoder.encode(text)),
    'memory://ownership-test.silk',
  )

describe('OwnershipView', () => {
  it('lists binding timelines with categories and live spans', () => {
    const facts = check('pub fn choose(left: I32, right: I32) -> I32 { return left }')
    expect(facts).toBeDefined()
    if (facts === undefined) return
    const markup = renderToStaticMarkup(<OwnershipView facts={facts} />)

    expect(markup).toContain('aria-label="Binding timeline of choose"')
    expect(markup).toMatch(/copyable · live \[\d+, \d+\) → \[\d+, \d+\)/)
    expect(markup).toContain('satisfied')
  })

  it('shows the empty-release return exit explicitly', () => {
    const facts = check('pub fn main() -> I32 { return 42 }')
    expect(facts).toBeDefined()
    if (facts === undefined) return
    const markup = renderToStaticMarkup(<OwnershipView facts={facts} />)

    expect(markup).toContain('exit return')
    expect(markup).toContain('releases none')
  })

  it('marks unavailable verdicts instead of claiming a satisfied check', () => {
    const facts = check('pub fn main() -> I32 { return missing() }')
    expect(facts).toBeDefined()
    if (facts === undefined) return
    const markup = renderToStaticMarkup(<OwnershipView facts={facts} />)

    expect(markup).toContain('verdict unavailable')
    expect(markup).not.toContain('>satisfied<')
  })
})

describe('OwnershipLab', () => {
  it('renders presets, editor, and the default fact view', () => {
    const markup = renderToStaticMarkup(<OwnershipLab />)

    expect(markup).toContain('aria-label="Ownership presets"')
    expect(markup).toContain('id="ownership-source"')
    expect(markup).toContain('exit return')
    expect(markup).toContain('No diagnostics')
  })
})
