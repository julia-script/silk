import { Analysis } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { CfgView } from '../mir-cfg/mir-cfg'
import { DiscoveryView, InstancesLab } from './instances'

const encoder = new TextEncoder()

const snapshot = (text: string): Analysis.Snapshot =>
  Analysis.ofSource('memory/instances-test', encoder.encode(text))

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

describe('DiscoveryView', () => {
  it('shows the resolved entry and discovery-ordered keys', () => {
    const discovery = Analysis.instancesOf(snapshot(nestedSource))
    const markup = renderToStaticMarkup(<DiscoveryView discovery={discovery} />)

    expect(markup).toContain('resolved · main')
    expect(markup.indexOf('memory/instances-test.main')).toBeLessThan(
      markup.indexOf('memory/instances-test.identity'),
    )
    expect(markup).toContain('&lt;&gt;[]')
  })

  it('shows an unavailable entry with its reason and no instances', () => {
    const discovery = Analysis.instancesOf(snapshot('pub fn answer() -> I32 { return 42 }'))
    const markup = renderToStaticMarkup(<DiscoveryView discovery={discovery} />)

    expect(markup).toContain('unavailable · MissingEntry')
    expect(markup).toContain('No instances')
  })
})

describe('CfgView over lowered programs', () => {
  it('renders lowered blocks with source-slice provenance', () => {
    const self = snapshot(nestedSource)
    const markup = renderToStaticMarkup(
      <CfgView module={Analysis.loweredMir(self)} source={nestedSource} />,
    )

    expect(markup).toContain('main · bb0')
    expect(markup).toContain('identity · bb0')
    expect(markup).toContain('call identity')
    expect(markup).toContain('· &quot; identity(identity(42))&quot;')
  })
})

describe('InstancesLab', () => {
  it('renders presets, editor, and the default discovery', () => {
    const markup = renderToStaticMarkup(<InstancesLab />)

    expect(markup).toContain('aria-label="Instance presets"')
    expect(markup).toContain('id="instances-source"')
    expect(markup).toContain('resolved · main')
  })
})
