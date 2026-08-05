import { Mir } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { CfgView, MirCfgLab } from './mir-cfg'

describe('CfgView', () => {
  it('lists blocks with kinds, operations, terminators, and edges', () => {
    const branching = Mir.samples().at(1)
    expect(branching).toBeDefined()
    if (branching === undefined) return
    const markup = renderToStaticMarkup(<CfgView module={branching} />)

    expect(markup).toContain('choose · bb0')
    expect(markup).toContain('choose · bb3 · cleanup')
    expect(markup).toContain('→ bb1 → bb2')
    expect(markup).toContain('branch %0 ? bb1 : bb2')
    expect(markup).toContain('trap ')
  })

  it('reveals provenance and generated markers on hover', () => {
    const branching = Mir.samples().at(1)
    expect(branching).toBeDefined()
    if (branching === undefined) return
    const markup = renderToStaticMarkup(<CfgView module={branching} />)

    expect(markup).toMatch(/title="\[\d+, \d+\) · generated"/)
    expect(markup).toMatch(/aria-label="drop %0 : \[\d+, \d+\) · generated"/)
  })
})

describe('MirCfgLab', () => {
  it('renders the sample picker and the encoded text', () => {
    const markup = renderToStaticMarkup(<MirCfgLab />)

    expect(markup).toContain('aria-label="MIR samples"')
    expect(markup).toContain('aria-label="Deterministic MIR encoding"')
    expect(markup).toContain('mir-module sample://straight.silk')
  })
})
