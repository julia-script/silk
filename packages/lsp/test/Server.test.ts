import { type ChildProcess, spawn } from 'node:child_process'
import { existsSync, mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'
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

const publishedDiagnosticReport = (
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
    assert.strictEqual(initialized.capabilities.definitionProvider, true)
    assert.strictEqual(initialized.capabilities.positionEncoding, 'utf-16')
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

it(
  'serves exact-version definitions and suppresses superseded diagnostics',
  { timeout: 30_000 },
  async () => {
    const client = connect()
    try {
      client.send({
        id: 1,
        method: 'initialize',
        params: {
          processId: null,
          rootUri: null,
          capabilities: {
            workspace: { didChangeWatchedFiles: { dynamicRegistration: true } },
          },
        },
      })
      await client.waitFor((message) => response(message, 1))
      client.send({ method: 'initialized', params: {} })
      const registration = await client.waitFor((message) =>
        message.method === 'client/registerCapability' && typeof message.id === 'number'
          ? { id: message.id, params: message.params }
          : undefined,
      )
      assert.include(JSON.stringify(registration.params), '**/*.silk')
      assert.include(JSON.stringify(registration.params), '**/silk.toml')
      client.send({ id: registration.id, result: null })

      const uri = 'file:///silk-lsp-e2e/coherent.silk'
      didOpen(client, uri, 'pub fn main() -> I32 { return missing() }')
      client.send({
        method: 'textDocument/didChange',
        params: {
          textDocument: { uri, version: 2 },
          contentChanges: [
            {
              text: `// 🧭
fn identity(value: I32) -> I32 { return value }
fn shadow() -> I32 {
  let value = 1
  if true {
    let value = 2
    return value
  }
  return value
}
pub fn main() -> I32 { return identity(42) }`,
            },
          ],
        },
      })
      const report = await client.waitFor((message) => {
        const candidate = publishedDiagnosticReport(message, uri)
        return candidate?.version === 2 ? candidate : undefined
      })
      assert.deepEqual(
        report.diagnostics.map((diagnostic) => diagnostic.code),
        ['SEM0008'],
      )

      client.send({
        id: 2,
        method: 'textDocument/definition',
        params: {
          textDocument: { uri },
          position: { line: 10, character: 'pub fn main() -> I32 { return '.length },
        },
      })
      const definitions = (await client.waitFor((message) => response(message, 2))) as Array<{
        targetUri: string
        targetSelectionRange: { start: { line: number; character: number } }
      }>
      assert.strictEqual(definitions.length, 1)
      assert.strictEqual(definitions[0]?.targetUri, uri)
      assert.deepEqual(definitions[0]?.targetSelectionRange.start, { line: 1, character: 3 })

      client.send({
        id: 3,
        method: 'textDocument/definition',
        params: {
          textDocument: { uri },
          position: { line: 6, character: '    return '.length },
        },
      })
      const shadowed = (await client.waitFor((message) => response(message, 3))) as Array<{
        targetSelectionRange: { start: { line: number; character: number } }
      }>
      // Rebinding is currently diagnosed and recovery selects the original declaration. The LSP
      // follows that compiler identity rather than inventing legal shadowing semantics.
      assert.deepEqual(shadowed[0]?.targetSelectionRange.start, { line: 3, character: 6 })

      client.send({
        method: 'textDocument/didClose',
        params: { textDocument: { uri } },
      })
      const cleared = await client.waitFor((message) => {
        const candidate = publishedDiagnosticReport(message, uri)
        return candidate !== undefined &&
          candidate.version === undefined &&
          candidate.diagnostics.length === 0
          ? candidate
          : undefined
      })
      assert.deepEqual(cleared.diagnostics, [])
    } finally {
      await client.close()
    }
  },
)

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

it(
  'navigates to closed and unsaved cross-file targets and invalidates disk dependencies',
  { timeout: 30_000 },
  async () => {
    const root = mkdtempSync(join(tmpdir(), 'silk-lsp-e2e-'))
    const sourceRoot = join(root, 'src')
    mkdirSync(sourceRoot)
    writeFileSync(
      join(root, 'silk.toml'),
      '[package]\nname = "navigation"\nroot = "src/Main.silk"\n',
    )
    const mainPath = join(sourceRoot, 'Main.silk')
    const utilPath = join(sourceRoot, 'Util.silk')
    const mainText = 'import Util\npub fn main() -> I32 { return Util.answer() }'
    writeFileSync(mainPath, mainText)
    writeFileSync(utilPath, 'pub fn answer() -> I32 { return 7 }')
    const mainUri = pathToFileURL(mainPath).href
    const utilUri = pathToFileURL(utilPath).href
    const client = connect()
    try {
      client.send({
        id: 1,
        method: 'initialize',
        params: { processId: null, rootUri: pathToFileURL(root).href, capabilities: {} },
      })
      await client.waitFor((message) => response(message, 1))
      client.send({ method: 'initialized', params: {} })
      didOpen(client, mainUri, mainText)
      await client.waitFor((message) => {
        const report = publishedDiagnosticReport(message, mainUri)
        return report?.version === 1 && report.diagnostics.length === 0 ? report : undefined
      })

      const requestDefinition = async (id: number) => {
        client.send({
          id,
          method: 'textDocument/definition',
          params: {
            textDocument: { uri: mainUri },
            position: { line: 1, character: mainText.split('\n')[1]?.indexOf('answer') ?? 0 },
          },
        })
        return (await client.waitFor((message) => response(message, id))) as Array<{
          targetUri: string
          targetSelectionRange: { start: { line: number; character: number } }
        }>
      }

      const closed = await requestDefinition(2)
      assert.strictEqual(closed[0]?.targetUri, utilUri)
      assert.strictEqual(closed[0]?.targetSelectionRange.start.line, 0)

      didOpen(client, utilUri, '\npub fn answer() -> I32 { return 8 }')
      await client.waitFor((message) => {
        const report = publishedDiagnosticReport(message, mainUri)
        return report?.diagnostics.length === 0 ? report : undefined
      })
      const unsaved = await requestDefinition(3)
      assert.strictEqual(unsaved[0]?.targetUri, utilUri)
      assert.strictEqual(unsaved[0]?.targetSelectionRange.start.line, 1)

      writeFileSync(utilPath, 'pub fn other() -> I32 { return 9 }')
      client.send({
        method: 'workspace/didChangeWatchedFiles',
        params: { changes: [{ uri: utilUri, type: 2 }] },
      })
      await new Promise((resolve) => setTimeout(resolve, 100))
      const staleDiskDiagnostics = client.messages.flatMap((message) => {
        const report = publishedDiagnosticReport(message, mainUri)
        return report === undefined ? [] : report.diagnostics
      })
      assert.isFalse(staleDiskDiagnostics.some((diagnostic) => diagnostic.code === 'SEM0014'))

      client.send({
        method: 'textDocument/didClose',
        params: { textDocument: { uri: utilUri } },
      })
      await new Promise((resolve) => setTimeout(resolve, 100))
      client.send({
        method: 'workspace/didChangeWatchedFiles',
        params: { changes: [{ uri: utilUri, type: 2 }] },
      })
      await client.waitFor((message) => {
        const report = publishedDiagnosticReport(message, mainUri)
        return report?.diagnostics.some((diagnostic) => diagnostic.code === 'SEM0014')
          ? report
          : undefined
      })

      writeFileSync(utilPath, 'pub fn answer() -> I32 { return 10 }')
      let nextMessage = client.messages.length
      client.send({
        method: 'workspace/didChangeWatchedFiles',
        params: { changes: [{ uri: utilUri, type: 2 }] },
      })
      await client.waitFor((message) => {
        if (client.messages.indexOf(message) < nextMessage) return undefined
        const report = publishedDiagnosticReport(message, mainUri)
        return report?.diagnostics.length === 0 ? report : undefined
      })

      const alternateRoot = join(root, 'alt')
      mkdirSync(alternateRoot)
      writeFileSync(join(alternateRoot, 'Entry.silk'), 'pub fn entry() -> I32 { return 0 }')
      writeFileSync(join(alternateRoot, 'Util.silk'), 'pub fn other() -> I32 { return 11 }')
      writeFileSync(
        join(root, 'silk.toml'),
        '[package]\nname = "navigation"\nroot = "alt/Entry.silk"\nsource-root = "alt"\n',
      )
      nextMessage = client.messages.length
      client.send({
        method: 'workspace/didChangeWatchedFiles',
        params: { changes: [{ uri: pathToFileURL(join(root, 'silk.toml')).href, type: 2 }] },
      })
      await client.waitFor((message) => {
        if (client.messages.indexOf(message) < nextMessage) return undefined
        const report = publishedDiagnosticReport(message, mainUri)
        return report?.diagnostics.some((diagnostic) => diagnostic.code === 'SEM0014')
          ? report
          : undefined
      })

      const reportsBeforeUnrelated = client.messages.filter(
        (message) => publishedDiagnosticReport(message, mainUri) !== undefined,
      ).length
      client.send({
        method: 'workspace/didChangeWatchedFiles',
        params: {
          changes: [{ uri: pathToFileURL(join(tmpdir(), 'unrelated.silk')).href, type: 2 }],
        },
      })
      await new Promise((resolve) => setTimeout(resolve, 100))
      const reportsAfterUnrelated = client.messages.filter(
        (message) => publishedDiagnosticReport(message, mainUri) !== undefined,
      ).length
      assert.strictEqual(reportsAfterUnrelated, reportsBeforeUnrelated)
    } finally {
      await client.close()
    }
  },
)
