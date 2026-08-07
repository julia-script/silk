import { type ChildProcess, spawn } from 'node:child_process'
import { existsSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'

const binPath = fileURLToPath(new URL('../dist/bin.js', import.meta.url))

/** A minimal Content-Length framed JSON-RPC client driving the real stdio server. */
interface Client {
  readonly child: ChildProcess
  readonly messages: Array<Record<string, unknown>>
  readonly send: (message: Record<string, unknown>) => void
  readonly waitFor: <A>(select: (message: Record<string, unknown>) => A | undefined) => Promise<A>
  readonly close: () => Promise<void>
}

const connect = (): Client => {
  const child = spawn(process.execPath, [binPath], { stdio: ['pipe', 'pipe', 'pipe'] })
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
        if (Date.now() - startedAt > 15_000) {
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

const response = (message: Record<string, unknown>, id: number): unknown =>
  message.id === id && 'result' in message ? message.result : undefined

const publishedDiagnostics = (
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

const didOpen = (client: Client, uri: string, text: string): void => {
  client.send({
    method: 'textDocument/didOpen',
    params: { textDocument: { uri, languageId: 'silk', version: 1, text } },
  })
}

it('serves diagnostics, hover, and formatting over real stdio', { timeout: 30_000 }, async () => {
  assert.isTrue(existsSync(binPath), 'dist/bin.js missing; run pnpm build first')
  const client = connect()
  try {
    client.send({
      id: 1,
      method: 'initialize',
      params: { processId: null, rootUri: null, capabilities: {} },
    })
    const initialized = (await client.waitFor((message) => response(message, 1))) as {
      capabilities: Record<string, unknown>
    }
    assert.strictEqual(initialized.capabilities.hoverProvider, true)
    assert.strictEqual(initialized.capabilities.documentFormattingProvider, true)
    client.send({ method: 'initialized', params: {} })

    const brokenUri = 'file:///silk-lsp-e2e/broken.silk'
    didOpen(client, brokenUri, 'pub fn main() -> I32 { return missing() }')
    const diagnostics = await client.waitFor((message) => publishedDiagnostics(message, brokenUri))
    assert.strictEqual(diagnostics.length, 1)
    assert.strictEqual(diagnostics[0]?.code, 'SEM0004')

    const hoverUri = 'file:///silk-lsp-e2e/hover.silk'
    const hoverText = 'pub fn main() -> I32 { return 42 }'
    didOpen(client, hoverUri, hoverText)
    await client.waitFor((message) => publishedDiagnostics(message, hoverUri))
    client.send({
      id: 2,
      method: 'textDocument/hover',
      params: {
        textDocument: { uri: hoverUri },
        position: { line: 0, character: hoverText.indexOf('42') },
      },
    })
    const hover = (await client.waitFor((message) => response(message, 2))) as {
      contents: { value: string }
    }
    assert.include(hover.contents.value, 'I32')

    const formatUri = 'file:///silk-lsp-e2e/format.silk'
    didOpen(client, formatUri, 'pub fn main() -> I32 { return   7 }')
    await client.waitFor((message) => publishedDiagnostics(message, formatUri))
    client.send({
      id: 3,
      method: 'textDocument/formatting',
      params: {
        textDocument: { uri: formatUri },
        options: { tabSize: 2, insertSpaces: true },
      },
    })
    const edits = (await client.waitFor((message) => response(message, 3))) as Array<{
      newText: string
    }>
    assert.strictEqual(edits.length, 1)
    assert.include(edits[0]?.newText, 'return 7')
  } finally {
    await client.close()
  }
})

it('refreshes sibling documents when an imported module changes', { timeout: 30_000 }, async () => {
  const client = connect()
  try {
    client.send({
      id: 1,
      method: 'initialize',
      params: { processId: null, rootUri: null, capabilities: {} },
    })
    await client.waitFor((message) => response(message, 1))
    client.send({ method: 'initialized', params: {} })

    const mainUri = 'file:///silk-lsp-e2e/multi/Main.silk'
    const utilUri = 'file:///silk-lsp-e2e/multi/Util.silk'
    didOpen(client, mainUri, 'import Util\npub fn main() -> I32 { return Util.answer() }')
    const alone = await client.waitFor((message) => publishedDiagnostics(message, mainUri))
    assert.strictEqual(alone[0]?.code, 'MOD0001')

    // Opening Util resolves the import through the overlay, so Main must republish clean.
    didOpen(client, utilUri, 'pub fn answer() -> I32 { return 7 }')
    await client.waitFor((message) => {
      const diagnostics = publishedDiagnostics(message, mainUri)
      return diagnostics !== undefined && diagnostics.length === 0 ? diagnostics : undefined
    })

    // Renaming the imported member breaks Main without Main changing at all.
    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri: utilUri, version: 2 },
        contentChanges: [{ text: 'pub fn other() -> I32 { return 7 }' }],
      },
    })
    const broken = await client.waitFor((message) => {
      const diagnostics = publishedDiagnostics(message, mainUri)
      return diagnostics?.some((diagnostic) => String(diagnostic.code).startsWith('SEM'))
        ? diagnostics
        : undefined
    })
    assert.isAtLeast(broken.length, 1)
  } finally {
    await client.close()
  }
})
