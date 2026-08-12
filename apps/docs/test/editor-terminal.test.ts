import { assert, it } from '@effect/vitest'
import { AsyncResult } from 'effect/unstable/reactivity'
import * as TerminalOutput from '../app/editor/TerminalOutput'

it('renders successful stream output while the stream is still waiting', () => {
  const failures: Array<unknown> = []
  const output: Array<string> = []

  TerminalOutput.consume(AsyncResult.success('terminal output', { waiting: true }), {
    onFailure: (failure) => failures.push(failure),
    onSuccess: (value) => output.push(value),
  })

  assert.deepStrictEqual(output, ['terminal output'])
  assert.deepStrictEqual(failures, [])
})
