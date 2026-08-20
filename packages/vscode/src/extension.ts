/**
 * The extension is a thin launcher: it forks the `@silk-effect/lsp` stdio server for `.silk`
 * documents and hands everything else to `vscode-languageclient`. All language behavior lives
 * in the server, so this file should never grow feature logic.
 */

import * as vscode from 'vscode'
import { LanguageClient, TransportKind } from 'vscode-languageclient/node'
import { registerInspector } from './inspector'

const createLifecycle = async (serverModule: string) => {
  const api = await import('./LanguageClientLifecycle.mjs')
  const owned = api.make(
    () =>
      new LanguageClient(
        'silk',
        'Silk Language Server',
        {
          run: { module: serverModule, transport: TransportKind.stdio },
          debug: { module: serverModule, transport: TransportKind.stdio },
        },
        {
          documentSelector: [{ language: 'silk' }],
        },
      ),
  )
  return { api, owned }
}

type LoadedLifecycle = Awaited<ReturnType<typeof createLifecycle>>

let lifecycle: Promise<LoadedLifecycle> | undefined
let inspectorClient: LanguageClient | undefined

const reportLifecycleFailure = (operation: string, error: unknown): void => {
  void vscode.window.showErrorMessage(`Silk language server ${operation} failed: ${String(error)}`)
}

export function activate(context: vscode.ExtensionContext): void {
  const serverModule = require.resolve('@silk-effect/lsp/bin')
  lifecycle = createLifecycle(serverModule)
  const owned = lifecycle
  context.subscriptions.push(
    vscode.commands.registerCommand('silk.restartLanguageServer', async () => {
      try {
        const loaded = await owned
        await loaded.api.runRestart(loaded.owned)
        inspectorClient = loaded.api.current(loaded.owned)
      } catch (error) {
        reportLifecycleFailure('restart', error)
      }
    }),
  )
  registerInspector(context, () => inspectorClient)
  void owned.then(async (loaded) => {
    try {
      await loaded.api.runStart(loaded.owned)
      inspectorClient = loaded.api.current(loaded.owned)
    } catch (error) {
      reportLifecycleFailure('start', error)
    }
  })
}

export function deactivate(): Thenable<void> | undefined {
  const owned = lifecycle
  lifecycle = undefined
  inspectorClient = undefined
  if (owned === undefined) return undefined
  return owned.then(async (loaded) => {
    try {
      await loaded.api.runStop(loaded.owned)
    } catch (error) {
      reportLifecycleFailure('stop', error)
    }
  })
}
