import { assert, it } from '@effect/vitest'
import * as Result from 'effect/Result'
import * as TerminalRenderer from '../app/editor/TerminalRenderer'

interface Host {
  readonly name: string
}

const makeHarness = () => {
  let activeInput: ((chunk: string) => void) | undefined
  let activeResize: (() => void) | undefined
  let backendDisposals = 0
  let inputDisposals = 0
  let observerDisposals = 0
  const writes: Array<string> = []

  const dependencies: TerminalRenderer.Dependencies<Host> = {
    createBackend: () => ({
      open: () => undefined,
      onData: (listener) => {
        activeInput = listener
        return {
          dispose: () => {
            inputDisposals += 1
          },
        }
      },
      write: (chunk) => {
        writes.push(chunk)
      },
      fit: () => undefined,
      dimensions: () => ({ cols: 90, rows: 28 }),
      dispose: () => {
        backendDisposals += 1
      },
    }),
    createObserver: (_host, onResize) => {
      activeResize = onResize
      return {
        dispose: () => {
          observerDisposals += 1
        },
      }
    },
  }

  return {
    dependencies,
    emitInput: (chunk: string) => activeInput?.(chunk),
    emitResize: () => activeResize?.(),
    counts: () => ({ backendDisposals, inputDisposals, observerDisposals }),
    writes,
  }
}

it('disposes every renderer resource exactly once and remounts safely', () => {
  const harness = makeHarness()
  const input: Array<string> = []
  const dimensions: Array<{ readonly cols: number; readonly rows: number }> = []
  const errors: Array<TerminalRenderer.TerminalRendererError> = []
  const options = {
    onInput: (chunk: string) => input.push(chunk),
    onResize: (size: { readonly cols: number; readonly rows: number }) => dimensions.push(size),
    onError: (error: TerminalRenderer.TerminalRendererError) => errors.push(error),
  }

  const first = TerminalRenderer.makeWith(harness.dependencies, { name: 'first' }, options)
  assert.isTrue(Result.isSuccess(first))
  if (Result.isFailure(first)) {
    return
  }
  first.success.write('hello')
  harness.emitInput('pwd\r')
  harness.emitResize()
  first.success.dispose()
  first.success.dispose()

  const second = TerminalRenderer.makeWith(harness.dependencies, { name: 'second' }, options)
  assert.isTrue(Result.isSuccess(second))
  if (Result.isFailure(second)) {
    return
  }
  second.success.dispose()

  assert.deepStrictEqual(input, ['pwd\r'])
  assert.deepStrictEqual(harness.writes, ['hello'])
  assert.deepStrictEqual(dimensions, [
    { cols: 90, rows: 28 },
    { cols: 90, rows: 28 },
    { cols: 90, rows: 28 },
  ])
  assert.deepStrictEqual(harness.counts(), {
    backendDisposals: 2,
    inputDisposals: 2,
    observerDisposals: 2,
  })
  assert.deepStrictEqual(errors, [])
})

it('returns initialization failures and reports later renderer failures', () => {
  const initialization = TerminalRenderer.makeWith(
    {
      createBackend: () => {
        throw new Error('renderer construction failed')
      },
      createObserver: () => ({ dispose: () => undefined }),
    },
    { name: 'broken' },
    {
      onInput: () => undefined,
      onResize: () => undefined,
      onError: () => undefined,
    },
  )
  assert.isTrue(Result.isFailure(initialization))

  const errors: Array<TerminalRenderer.TerminalRendererError> = []
  const renderer = TerminalRenderer.makeWith(
    {
      createBackend: () => ({
        open: () => undefined,
        onData: () => ({ dispose: () => undefined }),
        write: () => {
          throw new Error('write failed')
        },
        fit: () => undefined,
        dimensions: () => ({ cols: 80, rows: 24 }),
        dispose: () => undefined,
      }),
      createObserver: () => ({ dispose: () => undefined }),
    },
    { name: 'late-failure' },
    {
      onInput: () => undefined,
      onResize: () => undefined,
      onError: (error) => errors.push(error),
    },
  )
  assert.isTrue(Result.isSuccess(renderer))
  if (Result.isFailure(renderer)) {
    return
  }

  renderer.success.write('output')
  assert.strictEqual(errors.at(0)?.operation, 'write output')
})
