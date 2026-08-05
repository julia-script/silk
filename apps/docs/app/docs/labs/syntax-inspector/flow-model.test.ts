import { Lexer, Parser, SemanticAnalysis, SourceFile } from '@silk-effect/compiler'
import { describe, expect, it } from 'vitest'
import { projectDataFlow } from './flow-model'

const encoder = new TextEncoder()

const analyze = (id: string, text: string): SemanticAnalysis.Result =>
  SemanticAnalysis.analyze(Parser.parse(Lexer.lex(SourceFile.make(id, encoder.encode(text)))))

const identitySource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`

describe('projectDataFlow', () => {
  it('projects the complete literal-to-parameter-to-reference-to-return path', () => {
    const flow = projectDataFlow(analyze('memory://flow-complete.silk', identitySource))

    expect(flow.status).toBe('Complete')
    expect(flow.nodes.map((item) => item.kind)).toEqual([
      'Argument',
      'Parameter',
      'Reference',
      'CallResult',
      'FunctionReturn',
    ])
    expect(flow.nodes.map((item) => item.label)).toEqual([
      'Argument #0: 42',
      'identity.value',
      'Returned reference: value',
      'identity call result',
      'main return',
    ])
    expect(flow.edges.map((item) => item.label)).toEqual([
      'binds positionally to',
      'is read by',
      'produces',
      'is returned by',
    ])
    expect(flow.nodes.every((item) => item.span.sourceId === 'memory://flow-complete.silk')).toBe(
      true,
    )
  })

  it('keeps partial pairs and stops a wrong-arity path before a call result', () => {
    const flow = projectDataFlow(
      analyze(
        'memory://flow-arity.silk',
        `pub fn choose(left: I32, right: I32) -> I32 { return left }
pub fn main() -> I32 { return choose(1) }`,
      ),
    )

    expect(flow.status).toBe('Incomplete')
    expect(flow.edges.some((item) => item.label === 'binds positionally to')).toBe(true)
    expect(flow.nodes.some((item) => item.label === 'choose.right' && item.state === 'Unmatched')).toBe(
      true,
    )
    expect(flow.nodes.some((item) => item.kind === 'CallResult')).toBe(false)
    expect(flow.nodes.at(-1)?.label).toBe('Flow stops: ArityMismatch')
  })

  it('does not invent bindings for a missing call target', () => {
    const flow = projectDataFlow(
      analyze('memory://flow-missing.silk', 'pub fn main() -> I32 { return missing(42) }'),
    )

    expect(flow.status).toBe('Incomplete')
    expect(flow.edges.some((item) => item.label === 'binds positionally to')).toBe(false)
    expect(flow.nodes.some((item) => item.label === 'Flow stops: no unique target')).toBe(true)
  })

  it('branches to every ambiguous local parameter without selecting a result edge', () => {
    const flow = projectDataFlow(
      analyze(
        'memory://flow-ambiguous.silk',
        `pub fn choose(value: I32, value: I32) -> I32 { return value }
pub fn main() -> I32 { return choose(1, 2) }`,
      ),
    )

    expect(flow.status).toBe('Incomplete')
    expect(flow.edges.filter((item) => item.state === 'Branched')).toHaveLength(2)
    expect(flow.nodes.some((item) => item.kind === 'CallResult')).toBe(false)
  })

  it('stops at parser-owned damaged call syntax without inventing an argument', () => {
    const flow = projectDataFlow(
      analyze(
        'memory://flow-damaged.silk',
        `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(@) }`,
      ),
    )

    expect(flow.status).toBe('Incomplete')
    expect(flow.nodes.some((item) => item.kind === 'Argument')).toBe(false)
    expect(flow.nodes.at(-1)?.detail).toBe('UnavailableCallSyntax')
  })

  it('recomputes deterministically from equivalent disposable analysis state', () => {
    const first = projectDataFlow(analyze('memory://flow-repeat.silk', identitySource))
    const second = projectDataFlow(analyze('memory://flow-repeat.silk', identitySource))

    expect(first).toEqual(second)
  })
})
