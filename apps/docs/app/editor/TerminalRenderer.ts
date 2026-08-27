import { FitAddon } from '@xterm/addon-fit'
import { Terminal } from '@xterm/xterm'
import * as Data from 'effect/Data'
import * as Result from 'effect/Result'
import type * as WebContainerProcess from '@silk-lang/platform-webcontainer/WebContainerProcess'

interface Disposable {
  readonly dispose: () => void
}

export interface Backend extends Disposable {
  readonly open: () => void
  readonly onData: (listener: (chunk: string) => void) => Disposable
  readonly write: (chunk: string) => void
  readonly fit: () => void
  readonly dimensions: () => WebContainerProcess.Dimensions
}

export interface Observer extends Disposable {}

export interface Dependencies<Host> {
  readonly createBackend: (host: Host) => Backend
  readonly createObserver: (host: Host, onResize: () => void) => Observer
}

export interface Options {
  readonly onInput: (chunk: string) => void
  readonly onResize: (dimensions: WebContainerProcess.Dimensions) => void
  readonly onError: (error: TerminalRendererError) => void
}

export interface TerminalRenderer extends Disposable {
  readonly write: (chunk: string) => void
  readonly fit: () => void
}

export class TerminalRendererError extends Data.TaggedError('TerminalRendererError')<{
  readonly operation: string
  readonly message: string
  readonly cause: unknown
}> {}

const attempt = <A>(
  operation: string,
  evaluate: () => A,
): Result.Result<A, TerminalRendererError> =>
  Result.try({
    try: evaluate,
    catch: (cause) =>
      new TerminalRendererError({
        operation,
        message: `The terminal renderer could not ${operation}`,
        cause,
      }),
  })

const report = <A>(
  result: Result.Result<A, TerminalRendererError>,
  onError: Options['onError'],
) => {
  if (Result.isFailure(result)) {
    onError(result.failure)
  }
}

export const makeWith = <Host>(
  dependencies: Dependencies<Host>,
  host: Host,
  options: Options,
): Result.Result<TerminalRenderer, TerminalRendererError> => {
  const backendResult = attempt('initialize', () => dependencies.createBackend(host))
  if (Result.isFailure(backendResult)) {
    return Result.fail(backendResult.failure)
  }
  const backend = backendResult.success

  const opened = attempt('open', backend.open)
  if (Result.isFailure(opened)) {
    report(attempt('dispose after an open failure', backend.dispose), options.onError)
    return Result.fail(opened.failure)
  }

  const fit = () => {
    const fitted = attempt('fit', () => {
      backend.fit()
      const dimensions = backend.dimensions()
      if (dimensions.cols > 0 && dimensions.rows > 0) {
        options.onResize(dimensions)
      }
    })
    report(fitted, options.onError)
  }

  const inputResult = attempt('listen for input', () => backend.onData(options.onInput))
  if (Result.isFailure(inputResult)) {
    report(attempt('dispose after an input failure', backend.dispose), options.onError)
    return Result.fail(inputResult.failure)
  }
  const input = inputResult.success

  const observerResult = attempt('observe its size', () =>
    dependencies.createObserver(host, fit),
  )
  if (Result.isFailure(observerResult)) {
    report(attempt('remove input after an observer failure', input.dispose), options.onError)
    report(attempt('dispose after an observer failure', backend.dispose), options.onError)
    return Result.fail(observerResult.failure)
  }
  const observer = observerResult.success
  let disposed = false

  fit()

  return Result.succeed({
    write: (chunk) => report(attempt('write output', () => backend.write(chunk)), options.onError),
    fit,
    dispose: () => {
      if (disposed) {
        return
      }
      disposed = true
      report(attempt('disconnect its resize observer', observer.dispose), options.onError)
      report(attempt('remove its input listener', input.dispose), options.onError)
      report(attempt('dispose', backend.dispose), options.onError)
    },
  })
}

const liveDependencies: Dependencies<HTMLElement> = {
  createBackend: (host) => {
    const terminal = new Terminal({
      allowProposedApi: false,
      convertEol: true,
      cursorBlink: true,
      fontFamily: 'var(--font-mono, ui-monospace, SFMono-Regular, Menlo, monospace)',
      fontSize: 14,
      theme: {
        background: '#0b0d10',
        foreground: '#d7dde5',
        cursor: '#93c5fd',
        selectionBackground: '#334155',
      },
    })
    const fitAddon = new FitAddon()
    terminal.loadAddon(fitAddon)

    return {
      open: () => terminal.open(host),
      onData: (listener) => terminal.onData(listener),
      write: (chunk) => terminal.write(chunk),
      fit: () => fitAddon.fit(),
      dimensions: () => ({ cols: terminal.cols, rows: terminal.rows }),
      dispose: () => terminal.dispose(),
    }
  },
  createObserver: (host, onResize) => {
    const observer = new ResizeObserver(onResize)
    observer.observe(host)
    return { dispose: () => observer.disconnect() }
  },
}

export const make = (host: HTMLElement, options: Options) => makeWith(liveDependencies, host, options)
