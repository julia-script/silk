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

  it('shows the checked arithmetic expansion through the facade', () => {
    const snapshot = Analysis.ofSource(
      'memory://llvm-arith-test.silk',
      encoder.encode('pub fn main() -> I32 { return I32.divide(I32.add(40, 2), 1) }'),
    )
    const artifact = Analysis.codegen(snapshot, { mode: 'release' })

    expect(artifact.ir).toContain('llvm.sadd.with.overflow')
    expect(artifact.ir).toContain('arith_trap')
    expect(artifact.ir).toContain('sdiv')
    const mir = Analysis.loweredMir(snapshot)
    const operations = mir.functions.at(0)?.blocks.at(0)?.operations ?? []
    expect(
      operations.flatMap((operation) => (operation._tag === 'Binary' ? [operation.operator] : [])),
    ).toEqual(['Add', 'Divide'])
  })

  it('shows user-authored branch diamonds through the facade', () => {
    const snapshot = Analysis.ofSource(
      'memory://llvm-branch-test.silk',
      encoder.encode('pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'),
    )
    const mir = Analysis.loweredMir(snapshot)
    const main = mir.functions.at(0)

    expect(main?.blocks.length).toBe(3)
    expect(main?.blocks.at(0)?.terminator._tag).toBe('Branch')
    expect(Analysis.codegen(snapshot, { mode: 'release' }).ir).toContain('br i1')
    const outcome = Analysis.evaluate(snapshot)
    expect(outcome._tag).toBe('Completed')
    expect(outcome._tag === 'Completed' ? outcome.result.value : -1).toBe(42)
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
