import { assert, it } from '@effect/vitest'
import * as Prototype from './support/StaticRunnerInliningPrototype.js'

const accepted: Prototype.Graph = Object.freeze({
  entry: 4,
  locals: Object.freeze([2, 7, 9]),
  regions: Object.freeze([
    Object.freeze({
      _tag: 'OperationRegion',
      id: 4,
      operations: Object.freeze([
        Object.freeze({ _tag: 'Constant', destination: 2, value: 20 }),
        Object.freeze({ _tag: 'Constant', destination: 7, value: 22 }),
      ]),
      outcome: Object.freeze({ _tag: 'Forward', target: 8 }),
    }),
    Object.freeze({
      _tag: 'OperationRegion',
      id: 8,
      operations: Object.freeze([
        Object.freeze({ _tag: 'Add', destination: 9, left: 2, right: 7 }),
      ]),
      outcome: Object.freeze({ _tag: 'Return', value: 9 }),
    }),
  ]),
})

it('clones locals and regions deterministically and binds return to the continuation', () => {
  const options = Object.freeze({ firstLocal: 20, firstRegion: 30, continuation: 40, result: 50 })
  const first = Prototype.clone(accepted, options)
  const second = Prototype.clone(accepted, options)
  assert.deepEqual(first, second)
  assert.strictEqual(first._tag, 'Accepted')
  if (first._tag !== 'Accepted') return
  assert.deepEqual(
    [...first.localMap],
    [
      [2, 20],
      [7, 21],
      [9, 22],
    ],
  )
  assert.deepEqual(
    [...first.regionMap],
    [
      [4, 30],
      [8, 31],
    ],
  )
  assert.deepEqual(first.graph, {
    entry: 30,
    locals: [20, 21, 22],
    regions: [
      {
        _tag: 'OperationRegion',
        id: 30,
        operations: [
          { _tag: 'Constant', destination: 20, value: 20 },
          { _tag: 'Constant', destination: 21, value: 22 },
        ],
        outcome: { _tag: 'Forward', target: 31 },
      },
      {
        _tag: 'OperationRegion',
        id: 31,
        operations: [
          { _tag: 'Add', destination: 22, left: 20, right: 21 },
          { _tag: 'BindResult', destination: 50, source: 22 },
        ],
        outcome: { _tag: 'Forward', target: 40 },
      },
    ],
  })
})

const rejection = (graph: Prototype.Graph, reason: Prototype.Rejection) => {
  assert.deepEqual(
    Prototype.clone(graph, { firstLocal: 20, firstRegion: 30, continuation: 40, result: 50 }),
    { _tag: 'Rejected', reason },
  )
}

it('rejects locals and regions outside the closed graph', () => {
  rejection(
    {
      entry: 0,
      locals: [0],
      regions: [
        {
          _tag: 'OperationRegion',
          id: 0,
          operations: [{ _tag: 'Add', destination: 0, left: 0, right: 99 }],
          outcome: { _tag: 'Return', value: 0 },
        },
      ],
    },
    'UnknownLocal',
  )
  rejection({ entry: 99, locals: [], regions: [] }, 'UnknownRegion')
})

it('rejects conditional, loop, cleanup, and lexical-loop exits', () => {
  rejection(
    { entry: 0, locals: [], regions: [{ _tag: 'ConditionalRegion', id: 0 }] },
    'ConditionalOrLoopRegion',
  )
  rejection(
    { entry: 0, locals: [], regions: [{ _tag: 'LoopRegion', id: 0 }] },
    'ConditionalOrLoopRegion',
  )
  rejection({ entry: 0, locals: [], regions: [{ _tag: 'CleanupRegion', id: 0 }] }, 'CleanupRegion')
  rejection(
    {
      entry: 0,
      locals: [],
      regions: [{ _tag: 'OperationRegion', id: 0, operations: [], outcome: { _tag: 'Exit' } }],
    },
    'LexicalLoopExit',
  )
})

it('rejects cycles and graphs without exactly one return', () => {
  rejection(
    {
      entry: 0,
      locals: [],
      regions: [
        {
          _tag: 'OperationRegion',
          id: 0,
          operations: [],
          outcome: { _tag: 'Forward', target: 0 },
        },
      ],
    },
    'Cycle',
  )
  rejection(
    {
      entry: 0,
      locals: [0],
      regions: [
        {
          _tag: 'OperationRegion',
          id: 0,
          operations: [],
          outcome: { _tag: 'Return', value: 0 },
        },
        {
          _tag: 'OperationRegion',
          id: 1,
          operations: [],
          outcome: { _tag: 'Return', value: 0 },
        },
      ],
    },
    'MultipleReturns',
  )
})
