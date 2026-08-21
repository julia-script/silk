import { existsSync, mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath, pathToFileURL } from 'node:url'
import { assert, it } from '@effect/vitest'
import * as Document from '../src/Document.js'
import {
  binPath,
  connect,
  didOpen,
  failure,
  pulledDiagnosticReport,
  pulledDiagnostics,
  response,
} from './StdioClient.js'

const controlledBinPath = fileURLToPath(
  new URL('./fixtures/controlled-server.mjs', import.meta.url),
)
const wedgedBinPath = fileURLToPath(new URL('./fixtures/wedged-server.mjs', import.meta.url))

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
      legend: { tokenTypes: [...Document.semanticTokenTypes], tokenModifiers: [] },
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
    const hintText = `import silk.core { SystemAllocator }

pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return 0
}`
    didOpen(client, hintUri, hintText)
    await client.waitFor((message) => pulledDiagnostics(message, hintUri))
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
    const completionText = `import silk.effect as Effect

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
      'pub effect fn catch',
    )
  } finally {
    await client.close()
  }
})

it('uses pull diagnostics, stable result ids, and the synchronous acceptance barrier', {
  timeout: 30_000,
}, async () => {
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
})

it('cancels a same-document request while its exact revision is pending', async () => {
  const client = connect(controlledBinPath)
  try {
    client.send({
      id: 1,
      method: 'initialize',
      params: { processId: null, rootUri: null, capabilities: {} },
    })
    await client.waitFor((message) => response(message, 1))
    client.send({ method: 'initialized', params: {} })
    const uri = 'file:///silk-lsp-e2e/cancel/Main.silk'
    const baseline = 'pub fn main() -> i32 { return 1 }'
    didOpen(client, uri, baseline)
    await client.waitFor((message) => pulledDiagnosticReport(message, uri))

    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri, version: 2 },
        contentChanges: [{ text: 'pub fn main() -> i32 { return 2 }' }],
      },
    })
    client.send({
      id: 30,
      method: 'textDocument/hover',
      params: {
        textDocument: { uri },
        position: { line: 0, character: baseline.indexOf('1') },
      },
    })
    client.send({ method: '$/cancelRequest', params: { id: 30 } })
    assert.strictEqual(await client.waitFor((message) => response(message, 30)), null)
    await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, uri)
      return report?.version === 2 ? report : undefined
    })
    assert.strictEqual(
      client.messages.filter((message) => message.id === 30 && 'result' in message).length,
      1,
    )
  } finally {
    await client.close()
  }
})

it('pulls only the final diagnostics through effects to valid repair', {
  timeout: 30_000,
}, async () => {
  const client = connect(controlledBinPath)
  try {
    client.send({
      id: 1,
      method: 'initialize',
      params: { processId: null, rootUri: null, capabilities: {} },
    })
    await client.waitFor((message) => response(message, 1))
    client.send({ method: 'initialized', params: {} })
    const uri = 'file:///silk-lsp-e2e/repair/Main.silk'
    const source = (line: string) => `fn outer() -> Effect<i32> {
  let n = 123
  return effect { return n }
}
pub fn main() -> i32 {
${line}
  return run outer()
}`
    didOpen(client, uri, source(''))
    await client.waitFor((message) => pulledDiagnosticReport(message, uri))
    const marker = client.messages.length
    let version = 1
    for (const line of ['  effects', '  effec', '  Effect.', '']) {
      version += 1
      client.send({
        method: 'textDocument/didChange',
        params: {
          textDocument: { uri, version },
          contentChanges: [{ text: source(line) }],
        },
      })
    }
    const final = await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, uri)
      return report?.version === version ? report : undefined
    })
    assert.deepEqual(final.diagnostics, [])
    const applied = client.messages.slice(marker).flatMap((message) => {
      const report = pulledDiagnosticReport(message, uri)
      return report?.version === undefined ? [] : [report.version]
    })
    assert.deepEqual(applied, [version])
  } finally {
    await client.close()
  }
})

it('answers a healthy project and replaces a non-cooperative project worker', {
  timeout: 30_000,
}, async () => {
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
    await new Promise((resolve) => setTimeout(resolve, 1_000))
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
    const healthyIndex = client.messages.findIndex((message) => message.id === 40)

    const failureMessage = await client.waitFor((message) =>
      message.method === 'window/logMessage' && JSON.stringify(message.params).includes('incident')
        ? message
        : undefined,
    )
    const failureIndex = client.messages.indexOf(failureMessage)
    assert.isBelow(healthyIndex, failureIndex)
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
})

it('keeps a committed project live while another project accepts rapid slow edits', {
  timeout: 30_000,
}, async () => {
  const client = connect(controlledBinPath)
  try {
    client.send({
      id: 1,
      method: 'initialize',
      params: { processId: null, rootUri: null, capabilities: {} },
    })
    await client.waitFor((message) => response(message, 1))
    client.send({ method: 'initialized', params: {} })

    const readyUri = 'file:///silk-lsp-e2e/ready/Main.silk'
    const readyText = `pub fn main() -> i32 {
  let value = 42
  return value
}`
    didOpen(client, readyUri, readyText)
    await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, readyUri)
      return report?.version === 1 ? report : undefined
    })

    const slowUri = 'file:///silk-lsp-e2e/slow/Main.silk'
    didOpen(client, slowUri, 'pub fn main() -> i32 { return 1 }')
    await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, slowUri)
      return report?.version === 1 ? report : undefined
    })

    const marker = client.messages.length
    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri: slowUri, version: 2 },
        contentChanges: [{ text: 'pub fn main() -> i32 { return 2 }' }],
      },
    })
    await new Promise((resolve) => setTimeout(resolve, 15))
    client.send({
      id: 2,
      method: 'textDocument/hover',
      params: {
        textDocument: { uri: readyUri },
        position: { line: 1, character: '  let value = '.length },
      },
    })
    const hover = (await client.waitFor((message) => response(message, 2))) as {
      contents: { value: string }
    }
    assert.include(hover.contents.value, 'i32')
    const hoverIndex = client.messages.findIndex((message) => response(message, 2) !== undefined)
    const slowTwoIndex = client.messages.findIndex((message) => {
      const report = pulledDiagnosticReport(message, slowUri)
      return report?.version === 2
    })
    assert.isAtLeast(hoverIndex, marker)
    assert.strictEqual(slowTwoIndex, -1)

    for (let version = 3; version <= 5; version += 1) {
      client.send({
        method: 'textDocument/didChange',
        params: {
          textDocument: { uri: slowUri, version },
          contentChanges: [{ text: `pub fn main() -> i32 { return ${version} }` }],
        },
      })
    }
    client.send({
      id: 3,
      method: 'textDocument/inlayHint',
      params: {
        textDocument: { uri: readyUri },
        range: { start: { line: 0, character: 0 }, end: { line: 3, character: 1 } },
      },
    })
    const hints = (await client.waitFor((message) => response(message, 3))) as Array<{
      label: string
    }>
    assert.deepEqual(
      hints.map((hint) => hint.label),
      [': i32'],
    )
    await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, slowUri)
      return report?.version === 5 ? report : undefined
    })
    const rapidReports = client.messages.slice(marker).flatMap((message) => {
      const report = pulledDiagnosticReport(message, slowUri)
      return report?.version === undefined ? [] : [report.version]
    })
    assert.deepEqual(rapidReports, [5])
  } finally {
    await client.close()
  }
})

it('clears a failed version, recovers on the next edit, and shuts down after the defect', {
  timeout: 30_000,
}, async () => {
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

    const uri = 'file:///silk-lsp-e2e/failure/Main.silk'
    didOpen(client, uri, 'pub fn main() -> i32 { return missing() }')
    const baseline = await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, uri)
      return report?.version === 1 ? report : undefined
    })
    assert.deepEqual(
      baseline.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0004'],
    )

    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri, version: 2 },
        contentChanges: [{ text: '// LSP_TEST_DEFECT\npub fn main() -> i32 { return 1 }' }],
      },
    })
    const failed = await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, uri)
      return report?.version === 2 ? report : undefined
    })
    assert.deepEqual(failed.diagnostics, [])
    await client.waitFor((message) =>
      message.method === 'window/logMessage' && JSON.stringify(message.params).includes('incident')
        ? message
        : undefined,
    )
    client.send({
      id: 4,
      method: 'textDocument/hover',
      params: { textDocument: { uri }, position: { line: 1, character: 30 } },
    })
    assert.strictEqual(await client.waitFor((message) => response(message, 4)), null)

    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri, version: 3 },
        contentChanges: [{ text: 'pub fn main() -> i32 { return 1 }' }],
      },
    })
    const recovered = await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, uri)
      return report?.version === 3 ? report : undefined
    })
    assert.deepEqual(recovered.diagnostics, [])

    client.send({ id: 5, method: 'shutdown' })
    await client.waitFor((message) => response(message, 5))
    const exit = new Promise<void>((resolve) => client.child.once('exit', () => resolve()))
    client.send({ method: 'exit' })
    await exit
    exited = true
  } finally {
    if (!exited) await client.close()
  }
})

it('completes stdio shutdown during controlled active analysis', { timeout: 30_000 }, async () => {
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
    didOpen(client, 'file:///silk-lsp-e2e/shutdown/Main.silk', 'pub fn main() -> i32 { return 1 }')
    await new Promise((resolve) => setTimeout(resolve, 15))

    client.send({ id: 2, method: 'shutdown' })
    await client.waitFor((message) => response(message, 2))
    const exit = new Promise<void>((resolve) => client.child.once('exit', () => resolve()))
    client.send({ method: 'exit' })
    await exit
    exited = true
  } finally {
    if (!exited) await client.close()
  }
})

it('serves exact-version definitions and suppresses superseded diagnostics', {
  timeout: 30_000,
}, async () => {
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
    didOpen(client, uri, 'pub fn main() -> i32 { return missing() }')
    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri, version: 2 },
        contentChanges: [
          {
            text: `// 🧭
fn identity(value: i32) -> i32 { return value }
fn shadow() -> i32 {
  let value = 1
  if true {
    let value = 2
    return value
  }
  return value
}
pub fn main() -> i32 { return identity(42) }`,
          },
        ],
      },
    })
    const report = await client.waitFor((message) => {
      const candidate = pulledDiagnosticReport(message, uri)
      return candidate?.version === 2 ? candidate : undefined
    })
    assert.deepEqual(report.diagnostics, [])

    client.send({
      id: 2,
      method: 'textDocument/definition',
      params: {
        textDocument: { uri },
        position: { line: 10, character: 'pub fn main() -> i32 { return '.length },
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
    assert.deepEqual(shadowed[0]?.targetSelectionRange.start, { line: 5, character: 8 })

    const latest = `import silk.effect as Effect
import silk.i32 as i32

// π🙂
pub fn main() -> i32 {
  let allocator = i32.add(20, 22)
  return Effect.
}`
    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri, version: 3 },
        contentChanges: [{ text: latest }],
      },
    })
    await client.waitFor((message) => {
      const candidate = pulledDiagnosticReport(message, uri)
      return candidate?.version === 3 ? candidate : undefined
    })

    client.send({
      id: 4,
      method: 'textDocument/inlayHint',
      params: {
        textDocument: { uri },
        range: { start: { line: 0, character: 0 }, end: { line: 7, character: 1 } },
      },
    })
    const latestHints = (await client.waitFor((message) => response(message, 4))) as Array<{
      label: string
    }>
    assert.deepEqual(
      latestHints.map((hint) => hint.label),
      [': i32'],
    )

    client.send({
      id: 5,
      method: 'textDocument/completion',
      params: {
        textDocument: { uri },
        position: { line: 6, character: '  return Effect.'.length },
      },
    })
    const latestCompletion = (await client.waitFor((message) => response(message, 5))) as {
      items: Array<{ label: string }>
    }
    assert.include(
      latestCompletion.items.map((item) => item.label),
      'catch',
    )

    client.send({
      id: 6,
      method: 'textDocument/hover',
      params: {
        textDocument: { uri },
        position: { line: 5, character: '  let allocator = '.length },
      },
    })
    const latestHover = (await client.waitFor((message) => response(message, 6))) as {
      contents: { value: string }
    }
    assert.include(latestHover.contents.value, 'import silk/i32 as i32')

    client.send({
      method: 'textDocument/didClose',
      params: { textDocument: { uri } },
    })
    const cleared = await client.waitFor((message) => {
      const candidate = pulledDiagnosticReport(message, uri)
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
    didOpen(client, mainUri, 'import Util\npub fn main() -> i32 { return Util.answer() }')
    const alone = await client.waitFor((message) => pulledDiagnostics(message, mainUri))
    assert.strictEqual(alone[0]?.code, 'MOD0001')

    // Opening Util resolves the import through the overlay, so Main must republish clean.
    didOpen(client, utilUri, 'pub fn answer() -> i32 { return 7 }')
    await client.waitFor((message) => {
      const diagnostics = pulledDiagnostics(message, mainUri)
      return diagnostics !== undefined && diagnostics.length === 0 ? diagnostics : undefined
    })

    // Renaming the imported member breaks Main without Main changing at all.
    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri: utilUri, version: 2 },
        contentChanges: [{ text: 'pub fn other() -> i32 { return 7 }' }],
      },
    })
    const broken = await client.waitFor((message) => {
      const diagnostics = pulledDiagnostics(message, mainUri)
      return diagnostics?.some((diagnostic) => String(diagnostic.code).startsWith('SEM'))
        ? diagnostics
        : undefined
    })
    assert.isAtLeast(broken.length, 1)
  } finally {
    await client.close()
  }
})

it('navigates to closed and unsaved cross-file targets and invalidates disk dependencies', {
  timeout: 30_000,
}, async () => {
  const root = mkdtempSync(join(tmpdir(), 'silk-lsp-e2e-'))
  const sourceRoot = join(root, 'src')
  mkdirSync(sourceRoot)
  writeFileSync(
    join(root, 'silk.toml'),
    '[package]\nname = "navigation"\nversion = "0.1.0"\nroot = "src/Main.silk"\n',
  )
  const mainPath = join(sourceRoot, 'Main.silk')
  const utilPath = join(sourceRoot, 'Util.silk')
  const mainText = 'import Util\npub fn main() -> i32 { return Util.answer() }'
  writeFileSync(mainPath, mainText)
  writeFileSync(utilPath, 'pub fn answer() -> i32 { return 7 }')
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
      const report = pulledDiagnosticReport(message, mainUri)
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

    const overlayMarker = client.messages.length
    didOpen(client, utilUri, '\npub fn answer() -> i32 { return 8 }')
    await client.waitFor((message) => {
      if (client.messages.indexOf(message) < overlayMarker) return undefined
      const report = pulledDiagnosticReport(message, mainUri)
      return report?.diagnostics.length === 0 ? report : undefined
    })
    const unsaved = await requestDefinition(3)
    assert.strictEqual(unsaved[0]?.targetUri, utilUri)
    assert.strictEqual(unsaved[0]?.targetSelectionRange.start.line, 1)

    writeFileSync(utilPath, 'pub fn other() -> i32 { return 9 }')
    client.send({
      method: 'workspace/didChangeWatchedFiles',
      params: { changes: [{ uri: utilUri, type: 2 }] },
    })
    await new Promise((resolve) => setTimeout(resolve, 100))
    const staleDiskDiagnostics = client.messages.flatMap((message) => {
      const report = pulledDiagnosticReport(message, mainUri)
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
      const report = pulledDiagnosticReport(message, mainUri)
      return report?.diagnostics.some((diagnostic) => diagnostic.code === 'SEM0014')
        ? report
        : undefined
    })

    writeFileSync(utilPath, 'pub fn answer() -> i32 { return 10 }')
    let nextMessage = client.messages.length
    client.send({
      method: 'workspace/didChangeWatchedFiles',
      params: { changes: [{ uri: utilUri, type: 2 }] },
    })
    await client.waitFor((message) => {
      if (client.messages.indexOf(message) < nextMessage) return undefined
      const report = pulledDiagnosticReport(message, mainUri)
      return report?.diagnostics.length === 0 ? report : undefined
    })

    const alternateRoot = join(root, 'alt')
    mkdirSync(alternateRoot)
    writeFileSync(join(alternateRoot, 'Entry.silk'), 'pub fn entry() -> i32 { return 0 }')
    writeFileSync(join(alternateRoot, 'Util.silk'), 'pub fn other() -> i32 { return 11 }')
    writeFileSync(
      join(root, 'silk.toml'),
      '[package]\nname = "navigation"\nversion = "0.1.0"\nroot = "alt/Entry.silk"\nsource-root = "alt"\n',
    )
    nextMessage = client.messages.length
    client.send({
      method: 'workspace/didChangeWatchedFiles',
      params: { changes: [{ uri: pathToFileURL(join(root, 'silk.toml')).href, type: 2 }] },
    })
    await client.waitFor((message) => {
      if (client.messages.indexOf(message) < nextMessage) return undefined
      const report = pulledDiagnosticReport(message, mainUri)
      return report?.diagnostics.some((diagnostic) => diagnostic.code === 'SEM0014')
        ? report
        : undefined
    })

    const reportsBeforeUnrelated = client.messages.filter(
      (message) => pulledDiagnosticReport(message, mainUri) !== undefined,
    ).length
    client.send({
      method: 'workspace/didChangeWatchedFiles',
      params: {
        changes: [{ uri: pathToFileURL(join(tmpdir(), 'unrelated.silk')).href, type: 2 }],
      },
    })
    await new Promise((resolve) => setTimeout(resolve, 100))
    const reportsAfterUnrelated = client.messages.filter(
      (message) => pulledDiagnosticReport(message, mainUri) !== undefined,
    ).length
    assert.strictEqual(reportsAfterUnrelated, reportsBeforeUnrelated)
  } finally {
    await client.close()
  }
})

it('serves project-wide references and renames over real stdio', { timeout: 30_000 }, async () => {
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
    assert.strictEqual(initialized.capabilities.referencesProvider, true)
    assert.deepEqual(initialized.capabilities.renameProvider, { prepareProvider: true })
    client.send({ method: 'initialized', params: {} })

    const geometryUri = 'file:///silk-lsp-e2e/rename/Geometry.silk'
    const mainUri = 'file:///silk-lsp-e2e/rename/Main.silk'
    const geometryText =
      'pub fn area(width: i32, height: i32) -> i32 { return width }\npub fn perimeter() -> i32 { return 1 }'
    const mainText = 'import Geometry\npub fn main() -> i32 { return Geometry.area(2, 3) }'
    didOpen(client, geometryUri, geometryText)
    didOpen(client, mainUri, mainText)
    await client.waitFor((message) => {
      const diagnostics = pulledDiagnostics(message, mainUri)
      return diagnostics !== undefined && diagnostics.length === 0 ? diagnostics : undefined
    })

    const declaration = { line: 0, character: geometryText.indexOf('area') }
    client.send({
      id: 2,
      method: 'textDocument/references',
      params: {
        textDocument: { uri: geometryUri },
        position: declaration,
        context: { includeDeclaration: true },
      },
    })
    const locations = (await client.waitFor((message) => response(message, 2))) as Array<{
      uri: string
    }>
    assert.deepEqual(
      locations.map((location) => location.uri),
      [geometryUri, mainUri],
    )

    client.send({
      id: 3,
      method: 'textDocument/rename',
      params: {
        textDocument: { uri: geometryUri },
        position: declaration,
        newName: 'surface',
      },
    })
    const edit = (await client.waitFor((message) => response(message, 3))) as {
      changes: Record<string, Array<{ newText: string }>>
    }
    assert.deepEqual(Object.keys(edit.changes).sort(), [geometryUri, mainUri])
    assert.deepEqual(
      edit.changes[mainUri]?.map((change) => change.newText),
      ['surface'],
    )

    client.send({
      id: 4,
      method: 'textDocument/rename',
      params: { textDocument: { uri: geometryUri }, position: declaration, newName: 'perimeter' },
    })
    const refused = await client.waitFor((message) => failure(message, 4))
    assert.include(refused.message, 'SEM0016')

    client.send({
      id: 5,
      method: 'textDocument/prepareRename',
      params: { textDocument: { uri: geometryUri }, position: { line: 0, character: 1 } },
    })
    const unrenameable = await client.waitFor((message) => failure(message, 5))
    assert.include(unrenameable.message, 'no renameable declaration')

    // A name the installed toolchain declares is refused outright: applying the edit would rewrite
    // the standard library shipped with the compiler, for this project and every other one.
    const toolchainUri = 'file:///silk-lsp-e2e/rename/Toolchain.silk'
    const toolchainText =
      'import silk.bool { equals }\npub fn same() -> bool { return equals(true, true) }'
    didOpen(client, toolchainUri, toolchainText)
    await client.waitFor((message) => {
      const diagnostics = pulledDiagnostics(message, toolchainUri)
      return diagnostics !== undefined && diagnostics.length === 0 ? diagnostics : undefined
    })
    const imported = { line: 0, character: toolchainText.indexOf('equals') }

    client.send({
      id: 6,
      method: 'textDocument/rename',
      params: { textDocument: { uri: toolchainUri }, position: imported, newName: 'sameAs' },
    })
    const toolchainRefusal = await client.waitFor((message) => failure(message, 6))
    assert.include(toolchainRefusal.message, 'LSP0002')
    assert.include(toolchainRefusal.message, 'silk/bool')

    client.send({
      id: 7,
      method: 'textDocument/prepareRename',
      params: { textDocument: { uri: toolchainUri }, position: imported },
    })
    const unofferable = await client.waitFor((message) => failure(message, 7))
    assert.include(unofferable.message, 'no renameable declaration')

    // References stay read-only, so the toolchain declaration is still listed for the same name.
    client.send({
      id: 8,
      method: 'textDocument/references',
      params: {
        textDocument: { uri: toolchainUri },
        position: imported,
        context: { includeDeclaration: true },
      },
    })
    const toolchainLocations = (await client.waitFor((message) => response(message, 8))) as Array<{
      uri: string
    }>
    assert.isTrue(
      toolchainLocations.some((location) => location.uri.endsWith('/stdlib/silk/bool.silk')),
      JSON.stringify(toolchainLocations),
    )
  } finally {
    await client.close()
  }
})

it('offers a diagnostic edit as a quick fix over real stdio', { timeout: 30_000 }, async () => {
  const client = connect()
  try {
    client.send({
      id: 1,
      method: 'initialize',
      params: { processId: null, rootUri: null, capabilities: {} },
    })
    await client.waitFor((message) => response(message, 1))
    client.send({ method: 'initialized', params: {} })

    const geometryUri = 'file:///silk-lsp-e2e/fix/Geometry.silk'
    const mainUri = 'file:///silk-lsp-e2e/fix/Main.silk'
    const mainText = 'import Geometry as Geometry\npub fn main() -> i32 { return Geometry.area() }'
    didOpen(client, geometryUri, 'pub fn area() -> i32 { return 1 }')
    didOpen(client, mainUri, mainText)
    const diagnostics = await client.waitFor((message) => {
      const published = pulledDiagnostics(message, mainUri)
      return published !== undefined && published.length === 1 ? published : undefined
    })
    assert.strictEqual(diagnostics[0]?.code, 'LSP0002')

    client.send({
      id: 2,
      method: 'textDocument/codeAction',
      params: {
        textDocument: { uri: mainUri },
        range: { start: { line: 0, character: 0 }, end: { line: 0, character: mainText.length } },
        context: { diagnostics: [] },
      },
    })
    const actions = (await client.waitFor((message) => response(message, 2))) as Array<{
      title: string
      kind: string
      edit: { changes: Record<string, Array<{ range: unknown; newText: string }>> }
    }>
    assert.strictEqual(actions.length, 1)
    assert.strictEqual(actions[0]?.title, 'Remove the redundant alias')
    assert.strictEqual(actions[0]?.kind, 'quickfix')
    const clause = mainText.indexOf(' as Geometry')
    assert.deepEqual(actions[0]?.edit.changes[mainUri], [
      {
        range: {
          start: { line: 0, character: clause },
          end: { line: 0, character: clause + ' as Geometry'.length },
        },
        newText: '',
      },
    ])
  } finally {
    await client.close()
  }
})

it('discovers, resolves, and rejects stale auto-import actions over real stdio', {
  timeout: 30_000,
}, async () => {
  const root = mkdtempSync(join(tmpdir(), 'silk-lsp-auto-import-'))
  const sourceRoot = join(root, 'src')
  mkdirSync(sourceRoot)
  writeFileSync(
    join(root, 'silk.toml'),
    '[package]\nname = "imports"\nversion = "0.1.0"\nroot = "src/Main.silk"\n',
  )
  writeFileSync(join(sourceRoot, 'Alpha.silk'), 'pub fn calculate() -> i32 { return 1 }')
  writeFileSync(join(sourceRoot, 'Beta.silk'), 'pub fn calculate() -> i32 { return 2 }')
  const mainText = 'pub fn main() -> i32 { return calculate() }'
  const mainUri = pathToFileURL(join(sourceRoot, 'Main.silk')).href
  writeFileSync(join(sourceRoot, 'Main.silk'), mainText)
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
    await client.waitFor((message) => pulledDiagnostics(message, mainUri))

    client.send({
      id: 2,
      method: 'textDocument/codeAction',
      params: {
        textDocument: { uri: mainUri },
        range: {
          start: { line: 0, character: mainText.indexOf('calculate') },
          end: { line: 0, character: mainText.indexOf('calculate') + 'calculate'.length },
        },
        context: { diagnostics: [] },
      },
    })
    const actions = (await client.waitFor((message) => response(message, 2))) as Array<{
      title: string
      data: unknown
      edit?: { changes: Record<string, Array<{ newText: string }>> }
      disabled?: { reason: string }
    }>
    assert.deepEqual(
      actions.map((action) => action.title),
      ['Import calculate from Alpha', 'Import calculate from Beta'],
    )
    assert.deepEqual(
      actions[0]?.edit?.changes[mainUri]?.map((edit) => edit.newText),
      ['import Alpha { calculate }\n'],
    )
    const selected = actions[0]
    assert.isDefined(selected)
    if (selected === undefined) return

    client.send({ id: 3, method: 'codeAction/resolve', params: selected })
    const resolved = (await client.waitFor((message) => response(message, 3))) as {
      edit: { changes: Record<string, Array<{ newText: string }>> }
    }
    assert.deepEqual(
      resolved.edit.changes[mainUri]?.map((edit) => edit.newText),
      ['import Alpha { calculate }\n'],
    )

    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri: mainUri, version: 2 },
        contentChanges: [{ text: `// revised\n${mainText}` }],
      },
    })
    await client.waitFor((message) => {
      const report = pulledDiagnosticReport(message, mainUri)
      return report?.version === 2 ? report : undefined
    })
    client.send({ id: 4, method: 'codeAction/resolve', params: selected })
    const stale = (await client.waitFor((message) => response(message, 4))) as {
      disabled: { reason: string }
      edit?: unknown
    }
    assert.include(stale.disabled.reason, 'revision')
    assert.isUndefined(stale.edit)
  } finally {
    await client.close()
  }
})

it('resolves a standard-library auto-import using Silk source-path spelling', {
  timeout: 30_000,
}, async () => {
  const root = mkdtempSync(join(tmpdir(), 'silk-lsp-stdlib-auto-import-'))
  const sourceRoot = join(root, 'src')
  mkdirSync(sourceRoot)
  writeFileSync(
    join(root, 'silk.toml'),
    '[package]\nname = "imports"\nversion = "0.1.0"\nroot = "src/main.silk"\n',
  )
  const mainText = 'pub fn main() -> () {\n  Vector\n  return ()\n}\n'
  const mainUri = pathToFileURL(join(sourceRoot, 'main.silk')).href
  writeFileSync(join(sourceRoot, 'main.silk'), mainText)
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
    await client.waitFor((message) => pulledDiagnostics(message, mainUri))

    client.send({
      id: 2,
      method: 'textDocument/codeAction',
      params: {
        textDocument: { uri: mainUri },
        range: { start: { line: 1, character: 2 }, end: { line: 1, character: 8 } },
        context: { diagnostics: [] },
      },
    })
    const actions = (await client.waitFor((message) => response(message, 2))) as Array<{
      title: string
      data: unknown
      edit?: { changes: Record<string, Array<{ newText: string }>> }
    }>
    assert.deepEqual(
      actions.map((action) => action.title),
      ['Import Vector from silk/vector'],
    )
    assert.deepEqual(
      actions[0]?.edit?.changes[mainUri]?.map((edit) => edit.newText),
      ['import silk.vector { Vector }\n'],
    )
    const selected = actions[0]
    assert.isDefined(selected)
    if (selected === undefined) return

    client.send({ id: 3, method: 'codeAction/resolve', params: selected })
    const resolved = (await client.waitFor((message) => response(message, 3))) as {
      edit: { changes: Record<string, Array<{ newText: string }>> }
    }
    assert.deepEqual(
      resolved.edit.changes[mainUri]?.map((edit) => edit.newText),
      ['import silk.vector { Vector }\n'],
    )
  } finally {
    await client.close()
  }
})
