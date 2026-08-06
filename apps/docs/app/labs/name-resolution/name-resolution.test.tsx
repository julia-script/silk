import { Analysis } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { NameResolutionLab, ResolutionView } from './name-resolution'

const encoder = new TextEncoder()
describe('NameResolutionLab', () => {
  it('renders facade-provided canonical bindings', () => {
    const snapshot = Analysis.make({ rootModule: 'app/Main', sources: new Map([['app/Main', encoder.encode('import compiler.Syntax as Tree { parse }\npub fn main() -> I32 { return Tree.parse() }')], ['compiler/Syntax', encoder.encode('pub fn parse() -> I32 { return 42 }')]]) })
    const markup = renderToStaticMarkup(<ResolutionView resolution={Analysis.nameResolution(snapshot)} />)
    expect(markup).toContain('Tree')
    expect(markup).toContain('compiler/Syntax.parse')
  })
  it('offers the required inspection presets', () => {
    const markup = renderToStaticMarkup(<NameResolutionLab />)
    expect(markup).toContain('Hybrid alias')
    expect(markup).toContain('Private member')
    expect(markup).toContain('Collision')
    expect(markup).toContain('Cycle')
    expect(markup).toContain('Damaged alias')
    expect(markup).toContain('Unknown member')
  })
})
