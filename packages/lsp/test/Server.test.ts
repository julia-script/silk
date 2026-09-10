import { existsSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Document from '../src/Document.js'
import {
  binPath,
  connect,
  delay,
  didOpen,
  pulledDiagnosticReport,
  pulledDiagnostics,
  response,
  stdioTestTimeout,
  waitForExit,
} from './StdioClient.js'

const controlledBinPath = fileURLToPath(
  new URL('./fixtures/controlled-server.mjs', import.meta.url),
)
const wedgedBinPath = fileURLToPath(new URL('./fixtures/wedged-server.mjs', import.meta.url))

// Keep this file at the process boundary: one broad protocol canary, one pull-diagnostic state
// transition, one worker-fault recovery, and one shutdown. Document, Workspace, and
// WorkspaceEngine own the feature-specific behavior and cover it without starting another server.

it(
  'serves diagnostics, hover, and formatting over real stdio',
  { timeout: stdioTestTimeout },
  async () => {
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
      assert.strictEqual(initialized.capabilities.inlayHintProvider, true)
      assert.deepEqual(initialized.capabilities.completionProvider, { triggerCharacters: ['.'] })
      assert.strictEqual(initialized.capabilities.positionEncoding, 'utf-16')
      assert.strictEqual(initialized.capabilities.documentFormattingProvider, true)
      assert.deepEqual(initialized.capabilities.codeActionProvider, {
        codeActionKinds: ['quickfix'],
        resolveProvider: true,
      })
      assert.deepEqual(initialized.capabilities.signatureHelpProvider, {
        triggerCharacters: ['(', ','],
      })
      assert.strictEqual(initialized.capabilities.foldingRangeProvider, true)
      assert.strictEqual(initialized.capabilities.callHierarchyProvider, true)
      assert.deepEqual(initialized.capabilities.semanticTokensProvider, {
        legend: { tokenTypes: [...Document.semanticTokenTypes], tokenModifiers: ['inactive'] },
        full: true,
      })
      client.send({ method: 'initialized', params: {} })

      const brokenUri = 'file:///silk-lsp-e2e/broken.silk'
      didOpen(client, brokenUri, 'pub fn main() -> i32 { return missing() }')
      const diagnostics = await client.waitFor((message) => pulledDiagnostics(message, brokenUri))
      assert.strictEqual(diagnostics.length, 1)
      assert.strictEqual(diagnostics[0]?.code, 'SEM0004')

      const hoverUri = 'file:///silk-lsp-e2e/hover.silk'
      const hoverText = 'pub fn main() -> i32 { return 42 }'
      didOpen(client, hoverUri, hoverText)
      await client.waitFor((message) => pulledDiagnostics(message, hoverUri))
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
      assert.include(hover.contents.value, 'i32')

      const formatUri = 'file:///silk-lsp-e2e/format.silk'
      didOpen(client, formatUri, 'pub fn main() -> i32 { return   7 }')
      await client.waitFor((message) => pulledDiagnostics(message, formatUri))
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

      const hintUri = 'file:///silk-lsp-e2e/hints.silk'
      const hintText = `import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }

pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  return 0
}`
      didOpen(client, hintUri, hintText)
      await client.waitFor((message) => pulledDiagnostics(message, hintUri))
      await client.waitFor(
        (message) => (message.method === 'workspace/inlayHint/refresh' ? true : undefined),
        1_000,
      )
      client.send({
        id: 4,
        method: 'textDocument/inlayHint',
        params: {
          textDocument: { uri: hintUri },
          range: { start: { line: 0, character: 0 }, end: { line: 5, character: 1 } },
        },
      })
      const hints = (await client.waitFor((message) => response(message, 4))) as Array<{
        label: string
      }>
      assert.deepEqual(
        hints.map((hint) => hint.label),
        [': SystemAllocator'],
      )

      const completionUri = 'file:///silk-lsp-e2e/completion.silk'
      const completionText = `import silk.effect { Effect }

pub fn main() -> i32 {
  return Effect.
}`
      didOpen(client, completionUri, completionText)
      await client.waitFor((message) => pulledDiagnostics(message, completionUri))
      client.send({
        id: 5,
        method: 'textDocument/completion',
        params: {
          textDocument: { uri: completionUri },
          position: { line: 3, character: '  return Effect.'.length },
        },
      })
      const completion = (await client.waitFor((message) => response(message, 5))) as {
        items: Array<{ label: string; detail?: string }>
      }
      assert.include(
        completion.items.map((item) => item.label),
        'catch',
      )
      assert.include(
        completion.items.find((item) => item.label === 'catch')?.detail ?? '',
        "pub effect<'env> fn catch",
      )
    } finally {
      await client.close()
    }
  },
)

it(
  'uses pull diagnostics, stable result ids, and the synchronous acceptance barrier',
  {
    timeout: stdioTestTimeout,
  },
  async () => {
    const client = connect()
    try {
      client.send({
        id: 1,
        method: 'initialize',
        params: { processId: null, rootUri: null, capabilities: {} },
      })
      await client.waitFor((message) => response(message, 1))
      client.send({ method: 'initialized', params: {} })
      const uri = 'file:///silk-lsp-e2e/pull/Main.silk'
      didOpen(client, uri, 'pub fn main() -> i32 { return missing() }')
      await client.waitFor((message) => {
        const report = pulledDiagnosticReport(message, uri)
        return report?.diagnostics.length === 1 ? report : undefined
      })

      client.send({
        id: 20,
        method: 'silk/acceptedSourceVersions',
        params: { entries: [{ uri, version: 1 }] },
      })
      assert.deepEqual(await client.waitFor((message) => response(message, 20)), { accepted: true })

      client.send({
        id: 21,
        method: 'textDocument/diagnostic',
        params: { textDocument: { uri } },
      })
      const full = (await client.waitFor((message) => response(message, 21))) as {
        kind: string
        resultId: string
        items: ReadonlyArray<unknown>
      }
      assert.strictEqual(full.kind, 'full')
      assert.strictEqual(full.items.length, 1)
      client.send({
        id: 22,
        method: 'textDocument/diagnostic',
        params: { textDocument: { uri }, previousResultId: full.resultId },
      })
      assert.deepEqual(await client.waitFor((message) => response(message, 22)), {
        kind: 'unchanged',
        resultId: full.resultId,
      })
      assert.isFalse(
        client.messages.some((message) => message.method === 'textDocument/publishDiagnostics'),
      )
    } finally {
      await client.close()
    }
  },
)

it(
  'answers a healthy project and replaces a non-cooperative project worker',
  {
    timeout: stdioTestTimeout,
  },
  async () => {
    const client = connect(wedgedBinPath)
    try {
      client.send({
        id: 1,
        method: 'initialize',
        params: { processId: null, rootUri: null, capabilities: {} },
      })
      await client.waitFor((message) => response(message, 1))
      client.send({ method: 'initialized', params: {} })
      const wedgedUri = 'file:///silk-lsp-e2e/wedged/Main.silk'
      const healthyUri = 'file:///silk-lsp-e2e/healthy/Main.silk'
      didOpen(client, wedgedUri, 'pub fn main() -> i32 { return 1 }')
      await delay(1_000)
      didOpen(client, healthyUri, 'pub fn main() -> i32 { return 42 }')
      await client.waitFor((message) => {
        const report = pulledDiagnosticReport(message, healthyUri)
        return report?.diagnostics.length === 0 ? report : undefined
      })
      client.send({
        id: 40,
        method: 'textDocument/hover',
        params: {
          textDocument: { uri: healthyUri },
          position: { line: 0, character: 'pub fn main() -> i32 { return '.length },
        },
      })
      const healthy = (await client.waitFor((message) => response(message, 40))) as {
        contents: { value: string }
      } | null
      assert.isNotNull(healthy, JSON.stringify(client.messages))
      if (healthy === null) throw new Error('expected healthy hover')
      assert.include(healthy.contents.value, 'i32')

      const failureMessage = await client.waitFor((message) =>
        message.method === 'window/logMessage' &&
        JSON.stringify(message.params).includes('incident') &&
        JSON.stringify(message.params).includes('standalone:/silk-lsp-e2e/wedged')
          ? message
          : undefined,
      )
      const failureIndex = client.messages.indexOf(failureMessage)
      await client.waitFor((message) => {
        if (client.messages.indexOf(message) <= failureIndex) return undefined
        return message.method === 'silk/inspectorInvalidated' &&
          JSON.stringify(message.params).includes(wedgedUri)
          ? message
          : undefined
      })
      client.send({
        id: 41,
        method: 'textDocument/hover',
        params: {
          textDocument: { uri: wedgedUri },
          position: { line: 0, character: 'pub fn main() -> i32 { return '.length },
        },
      })
      const recovered = (await client.waitFor((message) => response(message, 41))) as {
        contents: { value: string }
      }
      assert.include(recovered.contents.value, 'i32')
    } finally {
      await client.close()
    }
  },
)

it(
  'completes stdio shutdown during controlled active analysis',
  { timeout: stdioTestTimeout },
  async () => {
    const client = connect(controlledBinPath)
    let exited = false
    try {
      client.send({
        id: 1,
        method: 'initialize',
        params: { processId: null, rootUri: null, capabilities: {} },
      })
      await client.waitFor((message) => response(message, 1))
      client.send({ method: 'initialized', params: {} })
      didOpen(
        client,
        'file:///silk-lsp-e2e/shutdown/Main.silk',
        'pub fn main() -> i32 { return 1 }',
      )
      await delay(15)

      client.send({ id: 2, method: 'shutdown' })
      await client.waitFor((message) => response(message, 2))
      const exit = waitForExit(client.child)
      client.send({ method: 'exit' })
      await exit
      exited = true
    } finally {
      if (!exited) await client.close()
    }
  },
)
