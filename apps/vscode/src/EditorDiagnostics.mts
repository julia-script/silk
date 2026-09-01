import * as Effect from 'effect/Effect'
import * as vscode from 'vscode'
import {
  DiagnosticRefreshRequest,
  DiagnosticServerCancellationData,
  DocumentDiagnosticReportKind,
  DocumentDiagnosticRequest,
  type LanguageClient,
  LSPCancellationError,
} from 'vscode-languageclient/node.js'
import type * as EditorSession from './EditorSession.mjs'

interface PullState {
  resultId: string | undefined
  request: vscode.CancellationTokenSource | undefined
}

export interface EditorDiagnostics {
  readonly start: () => void
  readonly clear: (uri: string) => void
  readonly dispose: () => void
}

/** Owns the only visible diagnostic collection and gates every pull at the editor edge. */
export const make = (options: {
  readonly client: LanguageClient
  readonly gate: EditorSession.DiagnosticGate
}): EditorDiagnostics => {
  const collection = vscode.languages.createDiagnosticCollection('silk')
  const states = new Map<string, PullState>()
  const bindings: Array<vscode.Disposable> = []
  let disposed = false
  let started = false

  const stateOf = (uri: string): PullState => {
    const current = states.get(uri)
    if (current !== undefined) return current
    const created: PullState = { resultId: undefined, request: undefined }
    states.set(uri, created)
    return created
  }

  const documentFor = (uri: string): vscode.TextDocument | undefined =>
    vscode.workspace.textDocuments.find((document) => document.uri.toString() === uri)

  const clear = (uri: string): void => {
    const state = states.get(uri)
    state?.request?.cancel()
    states.delete(uri)
    collection.delete(vscode.Uri.parse(uri))
  }

  const pull = (document: vscode.TextDocument): void => {
    if (disposed || document.languageId !== 'silk') return
    const uri = document.uri.toString()
    const version = document.version
    const state = stateOf(uri)
    state.request?.cancel()
    const request = new vscode.CancellationTokenSource()
    state.request = request
    void options.client
      .sendRequest(
        DocumentDiagnosticRequest.type,
        {
          textDocument: { uri },
          ...(state.resultId === undefined ? {} : { previousResultId: state.resultId }),
        },
        request.token,
      )
      .then(async (report) => {
        if (
          disposed ||
          request.token.isCancellationRequested ||
          state.request !== request ||
          !options.gate.isCurrent(uri, version)
        )
          return
        state.request = undefined
        state.resultId = report.resultId
        if (report.kind !== DocumentDiagnosticReportKind.Full) return
        const diagnostics = await options.client.protocol2CodeConverter.asDiagnostics(
          report.items,
          request.token,
        )
        if (
          disposed ||
          request.token.isCancellationRequested ||
          !options.gate.isCurrent(uri, version)
        )
          return
        collection.set(document.uri, diagnostics)
      })
      .catch((cause: unknown) => {
        if (disposed || request.token.isCancellationRequested) return
        state.request = undefined
        if (!options.gate.isCurrent(uri, version)) return
        const retrigger =
          cause instanceof LSPCancellationError &&
          DiagnosticServerCancellationData.is(cause.data) &&
          cause.data.retriggerRequest
        const current = retrigger ? documentFor(uri) : undefined
        if (current !== undefined)
          Effect.runFork(
            Effect.sleep(10).pipe(
              Effect.andThen(
                Effect.sync(() => {
                  if (!disposed && options.gate.isCurrent(uri, current.version)) pull(current)
                }),
              ),
            ),
          )
        else options.client.error(`Silk diagnostic pull failed for ${uri}`, cause, false)
      })
  }

  const start = (): void => {
    if (started || disposed) return
    started = true
    // vscode-languageclient's pull feature owns a private collection that cannot be synchronously
    // invalidated. Retire it after capability negotiation and keep one generation-gated owner.
    options.client.getFeature(DocumentDiagnosticRequest.method)?.clear()
    const change = options.client.getFeature('textDocument/didChange')
    const close = options.client.getFeature('textDocument/didClose')
    bindings.push(
      change.onNotificationSent((event) => pull(event.textDocument)),
      close.onNotificationSent((event) => clear(event.textDocument.uri.toString())),
      options.client.onRequest(DiagnosticRefreshRequest.type, async () => {
        for (const document of vscode.workspace.textDocuments) pull(document)
      }),
    )
    for (const document of vscode.workspace.textDocuments) pull(document)
  }

  const dispose = (): void => {
    if (disposed) return
    disposed = true
    for (const binding of bindings.splice(0)) binding.dispose()
    for (const state of states.values()) state.request?.cancel()
    states.clear()
    collection.dispose()
  }

  return Object.freeze({ start, clear, dispose })
}
