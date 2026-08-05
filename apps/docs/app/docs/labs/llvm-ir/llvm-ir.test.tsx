import { Analysis } from '@silk-effect/compiler'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { LlvmIrLab } from './llvm-ir'

const encoder = new TextEncoder()

describe('LlvmIrLab', () => {
  it('renders the emitted IR with silk_main and the symbol table', () => {
    const markup = renderToStaticMarkup(<LlvmIrLab />)

    expect(markup).toContain('aria-label="Emitted LLVM IR text"')
    expect(markup).toContain('silk_main')
    expect(markup).toContain('aria-label="Backend symbol table"')
    expect(markup).toContain('silk_1_identity')
    expect(markup).toContain('main · bb0')
  })

  it('answers debug emission with metadata through the facade', () => {
    const snapshot = Analysis.ofSource(
      'memory://llvm-ir-test.silk',
      encoder.encode('pub fn main() -> I32 { return 42 }'),
    )
    const debug = Analysis.codegen(snapshot, { mode: 'debug' })
    const release = Analysis.codegen(snapshot, { mode: 'release' })

    expect(debug.ir).toContain('!DICompileUnit(')
    expect(release.ir).not.toContain('DICompileUnit')
  })
})
