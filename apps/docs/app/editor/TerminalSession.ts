import * as WebContainer from '@silk-lang/platform-webcontainer/WebContainer'
import * as WebContainerError from '@silk-lang/platform-webcontainer/WebContainerError'
import * as WebContainerProcess from '@silk-lang/platform-webcontainer/WebContainerProcess'
import * as Effect from 'effect/Effect'
import * as Queue from 'effect/Queue'
import type * as Scope from 'effect/Scope'
import * as Stream from 'effect/Stream'
import { Atom } from 'effect/unstable/reactivity'
import * as EditorEnvironment from './EditorEnvironment'

export interface TerminalSession {
  readonly process: WebContainerProcess.Process
  readonly input: Queue.Queue<string>
}

/** Opens one shell and one continuous input writer for the enclosing editor scope. */
export const make = Effect.fn('TerminalSession.make')(function* (): Effect.fn.Return<
  TerminalSession,
  WebContainerError.WebContainerError,
  WebContainer.WebContainer | Scope.Scope
> {
  const process = yield* WebContainer.spawn('jsh', [], {
    terminal: { cols: 80, rows: 24 },
  })
  const input = yield* Effect.acquireRelease(Queue.bounded<string>(1024), Queue.shutdown)

  yield* Stream.fromQueue(input).pipe(Stream.run(process.input), Effect.forkScoped)

  return { process, input }
})

export const output = (self: TerminalSession) => self.process.output

export const write = Effect.fn('TerminalSession.write')(function* (
  self: TerminalSession,
  chunk: string,
) {
  const accepted = yield* Queue.offer(self.input, chunk)
  if (!accepted) {
    return yield* WebContainerError.invalidState(
      'TerminalSession.write',
      'Cannot write to a terminal session after its input has closed',
      'The terminal input queue is no longer accepting data',
    )
  }
})

export const resize = Effect.fn('TerminalSession.resize')(function* (
  self: TerminalSession,
  dimensions: WebContainerProcess.Dimensions,
) {
  return yield* WebContainerProcess.resize(self.process, dimensions)
})

/** The editor's one shell session; its default lifetime follows the mounted terminal surface. */
export const sessionAtom = EditorEnvironment.runtime
  .atom(make)
  .pipe(Atom.withServerValueInitial, Atom.withLabel('editor.terminal.session'))

/** The only consumer of the WebContainer process output stream. */
export const outputAtom = EditorEnvironment.runtime
  .atom((get) =>
    Stream.unwrap(get.result(sessionAtom).pipe(Effect.map((session) => output(session)))),
  )
  .pipe(Atom.withServerValueInitial, Atom.withLabel('editor.terminal.output'))

/** Enqueues terminal input without subscribing the caller to session state. */
export const writeAtom = EditorEnvironment.runtime
  .fn<string>()(
    Effect.fnUntraced(function* (chunk, get) {
      const session = yield* get.result(sessionAtom)
      return yield* write(session, chunk)
    }),
    { concurrent: true },
  )
  .pipe(Atom.withLabel('editor.terminal.write'))

/** Applies the latest fitted terminal dimensions to the active process. */
export const resizeAtom = EditorEnvironment.runtime
  .fn<WebContainerProcess.Dimensions>()(
    Effect.fnUntraced(function* (dimensions, get) {
      const session = yield* get.result(sessionAtom)
      return yield* resize(session, dimensions)
    }),
  )
  .pipe(Atom.withLabel('editor.terminal.resize'))
