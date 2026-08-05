import { Target, ToolchainPlan } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { ToolchainLab } from './toolchain'

describe('ToolchainLab', () => {
  it('shows the release object and link plans with structured arguments', () => {
    const markup = renderToStaticMarkup(<ToolchainLab />)

    expect(markup).toContain('aria-label="Planned toolchain commands"')
    expect(markup).toContain('--target=aarch64-apple-darwin')
    expect(markup).toContain('-c -x ir')
    expect(markup).toContain('-O2')
    expect(markup).toContain('silk_shim.o')
    expect(markup).toContain('aria-label="Runtime shim source"')
    expect(markup).toContain('return silk_main();')
    expect(markup).toContain('aria-label="Build scope lifecycle"')
  })

  it('plans debug and release-with-debug profiles distinctly', () => {
    const debug = ToolchainPlan.objectCommand(
      '/usr/bin/clang',
      Target.aarch64AppleDarwin,
      'debug',
      'in.bc',
      'out.o',
    )
    const releaseDebug = ToolchainPlan.objectCommand(
      '/usr/bin/clang',
      Target.aarch64AppleDarwin,
      'release-with-debug',
      'in.bc',
      'out.o',
    )

    expect(debug.arguments).toContain('-O0')
    expect(debug.arguments).toContain('-g')
    expect(releaseDebug.arguments).toContain('-O2')
    expect(releaseDebug.arguments).toContain('-g')
  })
})
