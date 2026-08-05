import { Analysis } from '@silk-effect/compiler'
import * as Effect from 'effect/Effect'
import { renderToStaticMarkup } from 'react-dom/server'
import { describe, expect, it } from 'vitest'
import { WasmLab } from './wasm'

const encoder = new TextEncoder()

const run = (source: string): number | 'trap' => {
  const snapshot = Analysis.ofSource(
    'memory://wasm-lab-test.silk',
    encoder.encode(source),
    'wasm32-unknown-unknown',
  )
  const artifact = Effect.runSync(Analysis.codegenWasm(snapshot, { mode: 'release' }))
  const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bitcode.slice()), {})
  try {
    return (instance.exports.silk_main as () => number)()
  } catch {
    return 'trap'
  }
}

describe('WasmLab', () => {
  it('renders the emitted WAT, the binary, and the execution result', () => {
    const markup = renderToStaticMarkup(<WasmLab />)

    expect(markup).toContain('aria-label="Emitted WebAssembly text"')
    expect(markup).toContain('aria-label="Emitted WebAssembly binary"')
    expect(markup).toContain('aria-label="WebAssembly execution result"')
    expect(markup).toContain('aria-label="Backend symbol table"')
    expect(markup).toContain('silk_main')
    expect(markup).toContain('silk_1_identity')
    expect(markup).toContain('main · bb0')
    expect(markup).toContain('aria-label="WebAssembly target layout plan"')
    expect(markup).toContain('wasm32-unknown-unknown')
  })

  it('emits a module the WebAssembly engine accepts and runs through the facade', () => {
    const snapshot = Analysis.ofSource(
      'memory://wasm-exec-test.silk',
      encoder.encode('pub fn main() -> I32 { return I32.divide(I32.add(40, 2), 1) }'),
      'wasm32-unknown-unknown',
    )
    const artifact = Effect.runSync(Analysis.codegenWasm(snapshot, { mode: 'release' }))

    expect(WebAssembly.validate(artifact.bitcode.slice())).toBe(true)
    expect(artifact.symbols.map((entry) => entry.symbol)).toEqual(['silk_main'])
  })

  it('recovers the source conditional as structured if/else', () => {
    const snapshot = Analysis.ofSource(
      'memory://wasm-branch-test.silk',
      encoder.encode('pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }'),
      'wasm32-unknown-unknown',
    )
    const artifact = Effect.runSync(Analysis.codegenWasm(snapshot, { mode: 'release' }))

    expect(artifact.ir).toMatch(/\bif\b/)
    expect(artifact.ir).toMatch(/\belse\b/)
    expect(artifact.ir).not.toContain('br_table')
  })

  it('agrees with the bootstrap interpreter on values and on traps', () => {
    expect(run('pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }')).toBe(42)
    expect(run('pub fn main() -> I32 { if I32.equals(1, 2) { return 0 } return 42 }')).toBe(42)
    expect(run('pub fn main() -> I32 { return I32.divide(1, 0) }')).toBe('trap')
    expect(run('pub fn main() -> I32 { return I32.add(2147483647, 1) }')).toBe('trap')
  })
})
