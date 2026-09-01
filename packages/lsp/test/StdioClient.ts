import { type ChildProcess, spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import * as Clock from 'effect/Clock'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Fiber from 'effect/Fiber'
import * as Inspectable from 'effect/Inspectable'

class StdioTimeout extends Data.TaggedError('StdioTimeout')<{
  readonly message: string
}> {}

/** Shared stdio harness: a minimal Content-Length framed JSON-RPC client driving the real server. */
export const binPath = fileURLToPath(new URL('../dist/bin.js', import.meta.url))

export const delay = (milliseconds: number): Promise<void> =>
  Effect.runPromise(Effect.sleep(milliseconds))

export const waitForExit = (child: ChildProcess): Promise<void> =>
  Effect.runPromise(
    Effect.callback<void>((resume) => {
      child.once('exit', () => resume(Effect.void))
    }),
  )

/** A minimal Content-Length framed JSON-RPC client driving the real stdio server. */
export interface Client {
  readonly child: ChildProcess
  readonly messages: Array<Record<string, unknown>>
  readonly send: (message: Record<string, unknown>) => void
  readonly waitFor: <A>(
    select: (message: Record<string, unknown>) => A | undefined,
    timeoutMilliseconds?: number,
  ) => Promise<A>
  readonly close: () => Promise<void>
}

interface DiagnosticRequest {
  readonly uri: string
  readonly version?: number
}

const diagnosticRequests = new Map<number, DiagnosticRequest>()
let nextDiagnosticRequest = 1_000_000

export const connect = (entryPath = binPath): Client => {
  const child = spawn(process.execPath, [entryPath], { stdio: ['pipe', 'pipe', 'pipe'] })
  const messages: Array<Record<string, unknown>> = []
  const openDocuments = new Map<string, number>()
  const pendingDiagnostics = new Set<string>()
  let buffer = Buffer.alloc(0)
  const rawSend = (message: Record<string, unknown>): void => {
    const body = JSON.stringify({ jsonrpc: '2.0', ...message })
    child.stdin.write(`Content-Length: ${Buffer.byteLength(body, 'utf8')}\r\n\r\n${body}`)
  }
  const pull = (uri: string, version?: number): void => {
    if (pendingDiagnostics.has(uri)) return
    pendingDiagnostics.add(uri)
    nextDiagnosticRequest += 1
    diagnosticRequests.set(nextDiagnosticRequest, {
      uri,
      ...(version === undefined ? {} : { version }),
    })
    rawSend({
      id: nextDiagnosticRequest,
      method: 'textDocument/diagnostic',
      params: { textDocument: { uri } },
    })
  }
  child.stdout.on('data', (chunk: Buffer) => {
    buffer = Buffer.concat([buffer, chunk])
    while (true) {
      const headerEnd = buffer.indexOf('\r\n\r\n')
      if (headerEnd === -1) return
      const header = buffer.subarray(0, headerEnd).toString('ascii')
      const match = /Content-Length: (\d+)/i.exec(header)
      if (match?.[1] === undefined) throw new Error(`Unframed server output: ${header}`)
      const length = Number(match[1])
      const bodyStart = headerEnd + 4
      if (buffer.length < bodyStart + length) return
      const message = JSON.parse(
        buffer.subarray(bodyStart, bodyStart + length).toString('utf8'),
      ) as Record<string, unknown>
      messages.push(message)
      buffer = buffer.subarray(bodyStart + length)
      if (typeof message.id === 'number') {
        const completed = diagnosticRequests.get(message.id)
        if (completed !== undefined) pendingDiagnostics.delete(completed.uri)
      }
      if (typeof message.id === 'number' && 'error' in message) {
        const request = diagnosticRequests.get(message.id)
        const error = message.error as { readonly data?: { readonly retriggerRequest?: boolean } }
        if (request !== undefined && error.data?.retriggerRequest === true)
          Effect.runFork(
            Effect.sleep(10).pipe(
              Effect.andThen(Effect.sync(() => pull(request.uri, openDocuments.get(request.uri)))),
            ),
          )
      }
      if (message.method === 'workspace/diagnostic/refresh' && message.id !== undefined) {
        rawSend({ id: message.id, result: null })
        for (const [uri, version] of openDocuments) pull(uri, version)
      }
      if (message.method === 'workspace/inlayHint/refresh' && message.id !== undefined)
        rawSend({ id: message.id, result: null })
    }
  })
  const send = (message: Record<string, unknown>): void => {
    let outbound = message
    if (message.method === 'initialize') {
      const parameters = (message.params ?? {}) as Record<string, unknown>
      const capabilities = (parameters.capabilities ?? {}) as Record<string, unknown>
      const workspace = (capabilities.workspace ?? {}) as Record<string, unknown>
      const textDocument = (capabilities.textDocument ?? {}) as Record<string, unknown>
      outbound = {
        ...message,
        params: {
          ...parameters,
          capabilities: {
            ...capabilities,
            workspace: {
              ...workspace,
              diagnostics: { refreshSupport: true },
              inlayHint: { refreshSupport: true },
            },
            textDocument: {
              ...textDocument,
              diagnostic: { dynamicRegistration: false, relatedDocumentSupport: false },
            },
          },
        },
      }
    }
    rawSend(outbound)
    if (message.method === 'textDocument/didOpen') {
      const parameters = message.params as {
        textDocument: { uri: string; version: number }
      }
      openDocuments.set(parameters.textDocument.uri, parameters.textDocument.version)
      pull(parameters.textDocument.uri, parameters.textDocument.version)
    } else if (message.method === 'textDocument/didChange') {
      const parameters = message.params as {
        textDocument: { uri: string; version: number }
      }
      openDocuments.set(parameters.textDocument.uri, parameters.textDocument.version)
      pull(parameters.textDocument.uri, parameters.textDocument.version)
    } else if (message.method === 'textDocument/didClose') {
      const parameters = message.params as { textDocument: { uri: string } }
      openDocuments.delete(parameters.textDocument.uri)
      pull(parameters.textDocument.uri)
    }
  }
  const waitFor = <A>(
    select: (message: Record<string, unknown>) => A | undefined,
    timeoutMilliseconds = 25_000,
  ): Promise<A> =>
    Effect.runPromise(
      Effect.gen(function* () {
        const startedAt = yield* Clock.currentTimeMillis
        while (true) {
          for (const message of messages) {
            const selected = select(message)
            if (selected !== undefined) return selected
          }
          // Kept under the 30s per-test budget: this inner poll must not be the thing that fires
          // first, or a slow runner reports a timeout instead of the test's own limit.
          if ((yield* Clock.currentTimeMillis) - startedAt > timeoutMilliseconds)
            return yield* new StdioTimeout({
              message: `Timed out waiting; saw ${Inspectable.toStringUnknown(messages)}`,
            })
          yield* Effect.sleep(25)
        }
      }),
    )
  const close = (): Promise<void> =>
    Effect.runPromise(
      Effect.gen(function* () {
        const exited = Effect.callback<void>((resume) => {
          child.once('exit', () => resume(Effect.void))
        })
        const exitFiber = yield* Effect.forkChild(exited)
        send({ id: 99, method: 'shutdown' })
        send({ method: 'exit' })
        const killFiber = yield* Effect.sleep(5_000).pipe(
          Effect.andThen(Effect.sync(() => child.kill())),
          Effect.forkChild,
        )
        yield* Fiber.join(exitFiber)
        yield* Fiber.interrupt(killFiber)
      }),
    )
  return { child, messages, send, waitFor, close }
}

export const response = (message: Record<string, unknown>, id: number): unknown =>
  message.id === id && 'result' in message ? message.result : undefined

export const failure = (
  message: Record<string, unknown>,
  id: number,
): { readonly message: string } | undefined =>
  message.id === id && 'error' in message
    ? (message.error as { readonly message: string })
    : undefined

export const pulledDiagnostics = (
  message: Record<string, unknown>,
  uri: string,
): ReadonlyArray<Record<string, unknown>> | undefined => {
  const report = pulledDiagnosticReport(message, uri)
  return report?.diagnostics
}

export const pulledDiagnosticReport = (
  message: Record<string, unknown>,
  uri: string,
):
  | {
      readonly version?: number
      readonly diagnostics: ReadonlyArray<Record<string, unknown>>
    }
  | undefined => {
  if (typeof message.id !== 'number' || !('result' in message)) return undefined
  const request = diagnosticRequests.get(message.id)
  if (request?.uri !== uri) return undefined
  const result = message.result as {
    readonly kind: 'full' | 'unchanged'
    readonly items?: ReadonlyArray<Record<string, unknown>>
  }
  return {
    ...(request.version === undefined ? {} : { version: request.version }),
    diagnostics: result.kind === 'full' ? (result.items ?? []) : [],
  }
}

export const didOpen = (client: Client, uri: string, text: string): void => {
  client.send({
    method: 'textDocument/didOpen',
    params: { textDocument: { uri, languageId: 'silk', version: 1, text } },
  })
}
