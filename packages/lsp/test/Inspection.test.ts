import { existsSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Inspection from '../src/Inspection.js'
import {
  binPath,
  connect,
  didOpen,
  failure,
  publishedDiagnostics,
  response,
} from './StdioClient.js'

interface ViewResult {
  readonly rows: ReadonlyArray<{
    readonly key: string
    readonly label: string
    readonly detail?: string
    readonly span?: { readonly module: string; readonly start: number; readonly end: number }
  }>
  readonly meta?: string
  readonly unavailable?: string
  readonly version: number
  readonly moduleUris: Readonly<Record<string, string>>
}

it('projects inspector views over real stdio', { timeout: 30_000 }, async () => {
  assert.isTrue(existsSync(binPath), 'dist/bin.js missing; run pnpm build first')
  const client = connect()
  try {
    client.send({
      id: 1,
      method: 'initialize',
      params: { processId: null, rootUri: null, capabilities: {} },
    })
    await client.waitFor((message) => response(message, 1))
    client.send({ method: 'initialized', params: {} })

    const uri = 'file:///silk-inspector-e2e/main.silk'
    const text = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`
    didOpen(client, uri, text)
    await client.waitFor((message) => publishedDiagnostics(message, uri))

    // The registry list is served for CJS clients that cannot import the ESM registry.
    client.send({ id: 9, method: Inspection.viewsRequest, params: {} })
    const listed = (await client.waitFor((message) => response(message, 9))) as {
      views: ReadonlyArray<{ id: string; title: string }>
    }
    assert.isTrue(listed.views.some((view) => view.id === 'tokens'))
    assert.isTrue(listed.views.some((view) => view.id === 'mir'))

    // A frontend view answers rows whose spans are module-qualified.
    client.send({ id: 2, method: Inspection.viewRequest, params: { uri, view: 'tokens' } })
    const tokens = (await client.waitFor((message) => response(message, 2))) as ViewResult
    assert.isAbove(tokens.rows.length, 0)
    assert.strictEqual(tokens.version, 1)
    const spanned = tokens.rows.find((row) => row.span !== undefined)
    assert.isDefined(spanned)
    assert.isString(spanned?.span?.module)
    assert.strictEqual(tokens.moduleUris[spanned?.span?.module ?? ''], uri)

    // A backend view realizes a single-root snapshot rooted at the document.
    client.send({ id: 3, method: Inspection.viewRequest, params: { uri, view: 'mir' } })
    const mir = (await client.waitFor((message) => response(message, 3))) as ViewResult
    assert.include(mir.meta ?? '', 'fn')
    assert.isTrue(mir.rows.some((row) => row.label.includes('main')))

    // Evaluation runs only when the request asks for it.
    client.send({ id: 4, method: Inspection.viewRequest, params: { uri, view: 'evaluation' } })
    const unevaluated = (await client.waitFor((message) => response(message, 4))) as ViewResult
    assert.strictEqual(unevaluated.meta, 'not run')
    client.send({
      id: 5,
      method: Inspection.viewRequest,
      params: { uri, view: 'evaluation', evaluate: true },
    })
    const evaluated = (await client.waitFor((message) => response(message, 5))) as ViewResult
    assert.include(evaluated.meta ?? '', 'steps')
    assert.isTrue(evaluated.rows.some((row) => (row.detail ?? '').includes('42')))

    // An unknown view id is an explicit error, not a crash or an empty result.
    client.send({ id: 6, method: Inspection.viewRequest, params: { uri, view: 'not-a-view' } })
    const unknown = await client.waitFor((message) => failure(message, 6))
    assert.include(unknown.message, 'not-a-view')

    // A commit announces itself, and a re-request answers the new revision.
    client.send({
      method: 'textDocument/didChange',
      params: {
        textDocument: { uri, version: 2 },
        contentChanges: [{ text: `pub fn main() -> i32 { return 7 }` }],
      },
    })
    // Every commit notifies (didOpen already produced version 1); wait for the edit's own.
    const invalidated = await client.waitFor((message) => {
      if (message.method !== Inspection.invalidatedNotification) return undefined
      const parameters = message.params as { uri: string; version: number }
      return parameters.version === 2 ? parameters : undefined
    })
    assert.strictEqual(invalidated.uri, uri)
    client.send({ id: 7, method: Inspection.viewRequest, params: { uri, view: 'tokens' } })
    const refreshed = (await client.waitFor((message) => response(message, 7))) as ViewResult
    assert.strictEqual(refreshed.version, 2)

    // A document outside any discovered project is an explicit error.
    client.send({
      id: 8,
      method: Inspection.viewRequest,
      params: { uri: 'file:///silk-inspector-e2e/never-opened.silk', view: 'tokens' },
    })
    const unopened = await client.waitFor((message) => failure(message, 8))
    assert.include(unopened.message, 'never-opened')
  } finally {
    await client.close()
  }
})
