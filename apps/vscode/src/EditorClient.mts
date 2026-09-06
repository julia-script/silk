import * as vscode from 'vscode'
import {
  DidChangeTextDocumentNotification,
  DidCloseTextDocumentNotification,
  DidOpenTextDocumentNotification,
  LanguageClient,
} from 'vscode-languageclient/node.js'
import * as EditorDiagnostics from './EditorDiagnostics.mjs'
import type * as EditorSession from './EditorSession.mjs'
import type * as ServerProcess from './ServerProcess.mjs'

const invalidatedNotification = 'silk/inspectorInvalidated'

/** Builds the concrete languageclient adapter hidden behind the stable editor session. */
export const make = (options: {
  readonly process: ServerProcess.ServerProcess
  readonly gate: EditorSession.DiagnosticGate
}): EditorSession.EditorClient => {
  const client = new LanguageClient(
    'silk',
    'Silk Language Server',
    async () => ({
      reader: options.process.reader,
      writer: options.process.writer,
      detached: true,
    }),
    {
      documentSelector: [{ language: 'silk' }],
      initializationOptions: () => vscode.workspace.getConfiguration('silk').get('analysis'),
      synchronize: { configurationSection: 'silk.analysis' },
    },
  )
  const diagnostics = EditorDiagnostics.make({ client, gate: options.gate })
  return Object.freeze({
    start: async () => {
      await client.start()
      diagnostics.start()
    },
    stop: async () => {
      diagnostics.dispose()
      await client.stop()
    },
    sendRequest: <A,>(method: string, parameters?: unknown) =>
      client.sendRequest<A>(method, parameters),
    onSourceSynchronization: (listener: (event: EditorSession.SourceSynchronization) => void) =>
      vscode.Disposable.from(
        client.getFeature(DidOpenTextDocumentNotification.method).onNotificationSent((event) =>
          listener({
            _tag: 'Open',
            uri: event.params.textDocument.uri,
            version: event.params.textDocument.version,
          }),
        ),
        client.getFeature(DidChangeTextDocumentNotification.method).onNotificationSent((event) =>
          listener({
            _tag: 'Change',
            uri: event.params.textDocument.uri,
            version: event.params.textDocument.version,
          }),
        ),
        client
          .getFeature(DidCloseTextDocumentNotification.method)
          .onNotificationSent((event) =>
            listener({ _tag: 'Close', uri: event.params.textDocument.uri }),
          ),
      ),
    onInvalidation: (listener: () => void) =>
      client.onNotification(invalidatedNotification, listener),
    deleteDiagnostics: diagnostics.clear,
  })
}
