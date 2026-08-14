import { assert, it } from '@effect/vitest'
import * as Option from 'effect/Option'
import * as SourceAction from '../src/SourceAction.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceSpan from '../src/SourceSpan.js'

const source = SourceFile.make('main', Uint8Array.from([1, 2, 3, 4]))
const span = (start: number, end: number) => Option.getOrThrow(SourceSpan.make(source, start, end))

it('constructs grouped, ordered, non-overlapping changes with source preconditions', () => {
  const plan = Option.getOrThrow(
    SourceAction.changePlan({
      preconditions: [SourceAction.precondition(source)],
      changes: [['main', [SourceAction.edit(span(3, 4), 'b'), SourceAction.edit(span(0, 1), 'a')]]],
    }),
  )

  assert.deepEqual(
    plan.changes.get('main')?.map((edit) => edit.span.start),
    [0, 3],
  )
  assert.strictEqual(SourceAction.isCurrent(plan, new Map([['main', source]])), true)
})

it('rejects overlapping or foreign edits', () => {
  assert.strictEqual(
    Option.isNone(
      SourceAction.changePlan({
        preconditions: [SourceAction.precondition(source)],
        changes: [['main', [SourceAction.edit(span(0, 2), ''), SourceAction.edit(span(1, 3), '')]]],
      }),
    ),
    true,
  )
})
