/**
 * The extension is a thin launcher: the editor session owns the `@silklang/lsp` process and
 * hands its detached stdio transport to `vscode-languageclient`. All language behavior lives in
 * the server, so this file should never grow feature logic.
 */

import * as vscode from 'vscode'
import { type InspectorSession, registerInspector } from './inspector'

const createSession = async (serverModule: string) => {
  const [api, clientApi, processApi, Effect] = await Promise.all([
    import('./EditorSession.mjs'),
    import('./EditorClient.mjs'),
    import('./ServerProcess.mjs'),
    import('effect/Effect'),
  ])
  const owned = api.make({
    currentDocuments: () =>
      vscode.workspace.textDocuments
        .filter((document) => document.languageId === 'silk')
        .map((document) => ({ uri: document.uri.toString(), version: document.version })),
    construct: (generation, gate) =>
      processApi.make({ module: serverModule }).pipe(
        Effect.mapError(
          (cause) =>
            new api.EditorSessionError({
              operation: 'construct',
              message: `Could not construct language-server generation ${generation}`,
              cause,
            }),
        ),
        Effect.map((process) => ({
          client: clientApi.make({ process, gate }),
          process: {
            awaitExit: process.awaitExit,
            terminate: process.terminate.pipe(
              Effect.mapError(
                (cause) =>
                  new api.EditorSessionError({
                    operation: 'retire',
                    message: `Could not retire language-server generation ${generation}`,
                    cause,
                  }),
              ),
            ),
          },
        })),
      ),
  })
  return { api, owned }
}

type LoadedSession = Awaited<ReturnType<typeof createSession>>

let session: Promise<LoadedSession> | undefined

const reportLifecycleFailure = (operation: string, error: unknown): void => {
  void vscode.window.showErrorMessage(`Silk language server ${operation} failed: ${String(error)}`)
}

export function activate(context: vscode.ExtensionContext): void {
  const serverModule = require.resolve('@silklang/lsp/bin')
  session = createSession(serverModule)
  const owned = session
  context.subscriptions.push(
    vscode.commands.registerCommand('silk.restartLanguageServer', async () => {
      try {
        const loaded = await owned
        await loaded.api.runRestart(loaded.owned)
      } catch (error) {
        reportLifecycleFailure('restart', error)
      }
    }),
    vscode.workspace.onDidChangeTextDocument((event) => {
      if (event.document.languageId !== 'silk') return
      void owned.then((loaded) =>
        loaded.api.documentChanged(loaded.owned, {
          uri: event.document.uri.toString(),
          version: event.document.version,
        }),
      )
    }),
    vscode.workspace.onDidCloseTextDocument((document) => {
      if (document.languageId !== 'silk') return
      void owned.then((loaded) => loaded.api.documentClosed(loaded.owned, document.uri.toString()))
    }),
  )
  const inspectorSession: InspectorSession = {
    request: async <A>(method: string, parameters?: unknown) => {
      const loaded = await owned
      return loaded.api.runRequest<A>(loaded.owned, method, parameters)
    },
    onInvalidation: (listener) => {
      let disposed = false
      let binding: { readonly dispose: () => void } | undefined
      void owned.then((loaded) => {
        if (disposed) return
        binding = loaded.api.onInvalidation(loaded.owned, listener)
      })
      return {
        dispose: () => {
          disposed = true
          binding?.dispose()
        },
      }
    },
  }
  registerInspector(context, inspectorSession)
  void owned.then(async (loaded) => {
    try {
      await loaded.api.runStart(loaded.owned)
    } catch (error) {
      reportLifecycleFailure('start', error)
    }
  })
}

export function deactivate(): Thenable<void> | undefined {
  const owned = session
  session = undefined
  if (owned === undefined) return undefined
  return owned.then(async (loaded) => {
    try {
      await loaded.api.runStop(loaded.owned)
    } catch (error) {
      reportLifecycleFailure('stop', error)
    }
  })
}
