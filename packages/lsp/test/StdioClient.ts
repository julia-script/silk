import { type ChildProcess, spawn } from 'node:child_process'
import { fileURLToPath } from 'node:url'

/** Shared stdio harness: a minimal Content-Length framed JSON-RPC client driving the real server. */
export const binPath = fileURLToPath(new URL('../dist/bin.js', import.meta.url))

/** A minimal Content-Length framed JSON-RPC client driving the real stdio server. */
export interface Client {
  readonly child: ChildProcess
  readonly messages: Array<Record<string, unknown>>
  readonly send: (message: Record<string, unknown>) => void
  readonly waitFor: <A>(select: (message: Record<string, unknown>) => A | undefined) => Promise<A>
  readonly close: () => Promise<void>
}

export const connect = (entryPath = binPath): Client => {
  const child = spawn(process.execPath, [entryPath], { stdio: ['pipe', 'pipe', 'pipe'] })
  const messages: Array<Record<string, unknown>> = []
  let buffer = Buffer.alloc(0)
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
      messages.push(JSON.parse(buffer.subarray(bodyStart, bodyStart + length).toString('utf8')))
      buffer = buffer.subarray(bodyStart + length)
    }
  })
  const send = (message: Record<string, unknown>): void => {
    const body = JSON.stringify({ jsonrpc: '2.0', ...message })
    child.stdin.write(`Content-Length: ${Buffer.byteLength(body, 'utf8')}\r\n\r\n${body}`)
  }
  const waitFor = <A>(select: (message: Record<string, unknown>) => A | undefined): Promise<A> =>
    new Promise((resolve, reject) => {
      const startedAt = Date.now()
      const poll = (): void => {
        for (const message of messages) {
          const selected = select(message)
          if (selected !== undefined) {
            resolve(selected)
            return
          }
        }
        // Kept under the 30s per-test budget: this inner poll must not be the thing that fires
        // first, or a slow runner reports a timeout instead of the test's own limit.
        if (Date.now() - startedAt > 25_000) {
          reject(new Error(`Timed out waiting; saw ${JSON.stringify(messages)}`))
          return
        }
        setTimeout(poll, 25)
      }
      poll()
    })
  const close = (): Promise<void> =>
    new Promise((resolve) => {
      child.once('exit', () => resolve())
      send({ id: 99, method: 'shutdown' })
      send({ method: 'exit' })
      setTimeout(() => child.kill(), 5_000).unref()
    })
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

export const publishedDiagnostics = (
  message: Record<string, unknown>,
  uri: string,
): ReadonlyArray<Record<string, unknown>> | undefined => {
  if (message.method !== 'textDocument/publishDiagnostics') return undefined
  const parameters = message.params as {
    uri: string
    diagnostics: Array<Record<string, unknown>>
  }
  return parameters.uri === uri ? parameters.diagnostics : undefined
}

export const publishedDiagnosticReport = (
  message: Record<string, unknown>,
  uri: string,
):
  | {
      readonly version?: number
      readonly diagnostics: ReadonlyArray<Record<string, unknown>>
    }
  | undefined => {
  if (message.method !== 'textDocument/publishDiagnostics') return undefined
  const parameters = message.params as {
    uri: string
    version?: number
    diagnostics: Array<Record<string, unknown>>
  }
  return parameters.uri === uri ? parameters : undefined
}

export const didOpen = (client: Client, uri: string, text: string): void => {
  client.send({
    method: 'textDocument/didOpen',
    params: { textDocument: { uri, languageId: 'silk', version: 1, text } },
  })
}
