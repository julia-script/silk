import * as Effect from 'effect/Effect'
import { describe, expect, it } from 'vitest'
import * as Analysis from '../src/Analysis.js'
import type * as Elaboration from '../src/Elaboration.js'
import { projectDataFlow } from '../src/InspectorFlowModel.js'

const encoder = new TextEncoder()

const snap = (id: string, text: string): Analysis.Snapshot =>
  Effect.runSync(Analysis.ofSourceRealized(id, encoder.encode(text)))

const analyze = (id: string, text: string): Elaboration.Result =>
  Analysis.rootAnalysis(snap(id, text))

const identitySource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`

const nestedSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`

const siblingSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(identity(1), identity(2)) }`

describe('projectDataFlow', () => {
  it('projects the canonical flat semantic path without evaluation claims', () => {
    const flow = projectDataFlow(analyze('memory/flow-complete', identitySource))

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
  })

  it('groups nested calls and connects the inner result to the outer argument', () => {
    const flow = projectDataFlow(analyze('memory/flow-nested', nestedSource))

    expect(flow.status).toBe('Complete')
    expect(flow.groups.map((group) => [group.depth, group.ordinal])).toEqual([
      [0, 0],
      [1, 0],
    ])
    expect(new Set(flow.groups.map((group) => group.id)).size).toBe(2)
    expect(flow.edges.some((item) => item.label === 'supplies nested result to')).toBe(true)
  })

  it('keeps repeated sibling call sites distinct and source ordered', () => {
    const flow = projectDataFlow(analyze('memory/flow-siblings', siblingSource))
    const children = flow.groups.filter((group) => group.depth === 1)

    expect(children.map((group) => group.ordinal)).toEqual([0, 1])
    expect(new Set(children.map((group) => group.id)).size).toBe(2)
  })

  it('terminates an unavailable inner type contract without inventing an enclosing result', () => {
    const flow = projectDataFlow(
      analyze(
        'memory/flow-missing',
        `pub fn identity(value: i32) -> i32 { return value }
pub fn uncertain(value: Mystery) -> i32 { return 0 }
pub fn main() -> i32 { return identity(uncertain(42)) }`,
      ),
    )

    expect(flow.status).toBe('Incomplete')
    expect(flow.groups).toHaveLength(2)
    expect(flow.nodes.some((item) => item.label === 'Data flow stops: Unavailable')).toBe(true)
    expect(flow.nodes.some((item) => item.kind === 'FunctionReturn')).toBe(false)
  })

  it('keeps positional facts but stops a wrong-arity nested call before results', () => {
    const flow = projectDataFlow(
      analyze(
        'memory/flow-arity',
        `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity()) }`,
      ),
    )

    expect(flow.status).toBe('Incomplete')
    expect(flow.nodes.some((item) => item.label === 'Data flow stops: ArityMismatch')).toBe(true)
    expect(flow.nodes.some((item) => item.kind === 'Parameter' && item.state === 'Unmatched')).toBe(
      true,
    )
    expect(flow.nodes.some((item) => item.kind === 'FunctionReturn')).toBe(false)
  })

  it('recomputes deterministically from equivalent disposable analysis state', () => {
    const first = projectDataFlow(analyze('memory/flow-repeat', identitySource))
    const second = projectDataFlow(analyze('memory/flow-repeat', identitySource))

    expect(first).toEqual(second)
  })
})
