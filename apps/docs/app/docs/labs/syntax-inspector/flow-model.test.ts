import { Analysis } from '@silk-effect/compiler'
import type { Elaboration } from '@silk-effect/compiler'
import { describe, expect, it } from 'vitest'
import { projectDataFlow } from './flow-model'

const encoder = new TextEncoder()

const snap = (id: string, text: string): Analysis.Snapshot =>
  Analysis.ofSource(id, encoder.encode(text))

const analyze = (id: string, text: string): Elaboration.Result =>
  Analysis.rootAnalysis(snap(id, text))

const identitySource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`

const nestedSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`

const siblingSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), identity(2)) }`

describe('projectDataFlow', () => {
  it('projects the canonical flat semantic path without evaluation claims', () => {
    const flow = projectDataFlow(analyze('memory://flow-complete.silk', identitySource))

    expect(flow.mode).toBe('Semantic')
    expect(flow.status).toBe('Complete')
    expect(flow.groups).toHaveLength(1)
    expect(flow.nodes.map((item) => item.kind)).toEqual([
      'Argument',
      'Parameter',
      'Reference',
      'CallResult',
      'FunctionReturn',
    ])
    expect(flow.edges.map((item) => item.label)).toEqual([
      'binds positionally to',
      'is read by',
      'produces',
      'is returned by',
    ])
    expect(flow.groups.every((item) => item.evaluation === undefined)).toBe(true)
    expect(flow.nodes.every((item) => item.evaluation === undefined)).toBe(true)
  })

  it('groups nested calls and connects the inner result to the outer argument', () => {
    const flow = projectDataFlow(analyze('memory://flow-nested.silk', nestedSource))

    expect(flow.status).toBe('Complete')
    expect(flow.groups.map((group) => [group.depth, group.ordinal])).toEqual([
      [0, 0],
      [1, 0],
    ])
    expect(new Set(flow.groups.map((group) => group.id)).size).toBe(2)
    expect(flow.edges.some((item) => item.label === 'supplies nested result to')).toBe(true)
  })

  it('keeps repeated sibling call sites distinct and source ordered', () => {
    const snapshot = snap('memory://flow-siblings.silk', siblingSource)
    const analysis = Analysis.rootAnalysis(snapshot)
    const outcome = Analysis.evaluate(snapshot)
    const flow = projectDataFlow(analysis, outcome)
    const children = flow.groups.filter((group) => group.depth === 1)

    expect(children.map((group) => group.ordinal)).toEqual([0, 1])
    expect(new Set(children.map((group) => group.id)).size).toBe(2)
    expect(children.map((group) => group.evaluation?.order)).toEqual([3, 7])
    expect(flow.mode).toBe('Evaluated')
    expect(flow.nodes.some((item) => item.evaluation?.value === 2)).toBe(true)
  })

  it('terminates an unavailable inner type contract without inventing an enclosing result', () => {
    const flow = projectDataFlow(
      analyze(
        'memory://flow-missing.silk',
        `pub fn identity(value: I32) -> I32 { return value }
pub fn uncertain(value: Mystery) -> I32 { return 0 }
pub fn main() -> I32 { return identity(uncertain(42)) }`,
      ),
    )

    expect(flow.status).toBe('Incomplete')
    expect(flow.groups).toHaveLength(2)
    expect(flow.nodes.some((item) => item.label === 'Flow stops: Unavailable')).toBe(true)
    expect(flow.nodes.some((item) => item.kind === 'FunctionReturn')).toBe(false)
  })

  it('keeps positional facts but stops a wrong-arity nested call before results', () => {
    const flow = projectDataFlow(
      analyze(
        'memory://flow-arity.silk',
        `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity()) }`,
      ),
    )

    expect(flow.status).toBe('Incomplete')
    expect(flow.nodes.some((item) => item.label === 'Flow stops: ArityMismatch')).toBe(true)
    expect(
      flow.nodes.some(
        (item) => item.kind === 'Parameter' && item.state === 'Unmatched',
      ),
    ).toBe(true)
    expect(flow.nodes.some((item) => item.kind === 'FunctionReturn')).toBe(false)
  })

  it('overlays only the completed prefix when a later sibling blocks', () => {
    const snapshot = snap('memory://flow-blocked.silk', `pub fn identity(value: I32) -> I32 { return value }
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), missing(2)) }`)
    const analysis = Analysis.rootAnalysis(snapshot)
    const outcome = Analysis.evaluate(snapshot)
    const flow = projectDataFlow(analysis, outcome)
    const outer = flow.groups.find((group) => group.depth === 0)

    expect(outcome._tag).toBe('Blocked')
    expect(flow.nodes.some((item) => item.evaluation?.value === 1)).toBe(true)
    expect(flow.nodes.some((item) => item.label === 'Evaluation stops: MissingCallTarget')).toBe(true)
    expect(
      flow.edges.some(
        (item) => item.groupId === outer?.id && item.label === 'binds positionally to' && item.evaluation !== undefined,
      ),
    ).toBe(false)
    expect(flow.nodes.some((item) => item.kind === 'FunctionReturn' && item.evaluation !== undefined)).toBe(false)
  })

  it('renders a recursive cycle as one finite trace-backed terminal', () => {
    const snapshot = snap('memory://flow-cycle.silk', `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(main()) }`)
    const analysis = Analysis.rootAnalysis(snapshot)
    const outcome = Analysis.evaluate(snapshot)
    const flow = projectDataFlow(analysis, outcome)
    const terminals = flow.nodes.filter(
      (item) => item.layer === 'Evaluated' && item.kind === 'Terminal',
    )

    expect(outcome._tag).toBe('Blocked')
    expect(terminals).toHaveLength(1)
    expect(terminals.at(0)?.detail).toContain('main → main')
    expect(flow.nodes.some((item) => item.kind === 'FunctionReturn' && item.evaluation !== undefined)).toBe(false)
  })

  it('recomputes deterministically from equivalent disposable analysis state', () => {
    const first = projectDataFlow(analyze('memory://flow-repeat.silk', identitySource))
    const second = projectDataFlow(analyze('memory://flow-repeat.silk', identitySource))

    expect(first).toEqual(second)
  })
})
