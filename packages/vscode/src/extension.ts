/**
 * The extension is a thin launcher: it forks the `@silk-effect/lsp` stdio server for `.silk`
 * documents and hands everything else to `vscode-languageclient`. All language behavior lives
 * in the server, so this file should never grow feature logic.
 */
import * as vscode from 'vscode'
import { LanguageClient, TransportKind } from 'vscode-languageclient/node'

let client: LanguageClient | undefined

export function activate(context: vscode.ExtensionContext): void {
  const serverModule = require.resolve('@silk-effect/lsp/bin')
  client = new LanguageClient(
    'silk',
    'Silk Language Server',
    {
      run: { module: serverModule, transport: TransportKind.stdio },
      debug: { module: serverModule, transport: TransportKind.stdio },
    },
    {
      documentSelector: [{ language: 'silk' }],
    },
  )
  // Respawns the server from the current dist, so a rebuilt server lands without a window reload.
  context.subscriptions.push(
    vscode.commands.registerCommand('silk.restartLanguageServer', () => client?.restart()),
  )
  void client.start()
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop()
}
