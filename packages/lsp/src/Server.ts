import { NodeServices } from '@effect/platform-node'
import * as Analysis from '@silk-effect/compiler/Analysis'
import * as Effect from 'effect/Effect'
import type * as FileSystem from 'effect/FileSystem'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import type * as Path from 'effect/Path'
import {
  createConnection,
  ProposedFeatures,
  TextDocumentSyncKind,
  TextDocuments,
} from 'vscode-languageserver/node.js'
import { TextDocument } from 'vscode-languageserver-textdocument'
import * as Document from './Document.js'
import * as Workspace from './Workspace.js'

const encoder = new TextEncoder()

/** One analyzed state of an open document, replaced wholesale on every change. */
interface Session {
  readonly document: Document.Document
  readonly snapshot: Analysis.Snapshot
}

/**
 * The external protocol boundary. `vscode-languageserver` is callback-driven, so this module
 * is the application edge: each handler builds one Effect and runs it against the Node
 * platform services, keeping every module inward of this file effectful and platform-free.
 */
export const start = (): void => {
  const connection = createConnection(ProposedFeatures.all, process.stdin, process.stdout)
  const documents = new TextDocuments(TextDocument)
  const sessions = new Map<string, Session>()
  const generations = new Map<string, number>()

  // Platform services are built once for the server's lifetime, not per request.
  const runtime = ManagedRuntime.make(NodeServices.layer)
  const run = <A>(effect: Effect.Effect<A, never, FileSystem.FileSystem | Path.Path>): Promise<A> =>
    runtime.runPromise(effect)

  connection.onInitialize(() => ({
    capabilities: {
      positionEncoding: 'utf-16',
      textDocumentSync: TextDocumentSyncKind.Incremental,
      hoverProvider: true,
      documentSymbolProvider: true,
      documentFormattingProvider: true,
    },
  }))

  const refresh = (text: TextDocument): Promise<void> => {
    const generation = (generations.get(text.uri) ?? 0) + 1
    generations.set(text.uri, generation)
    return run(
      Effect.gen(function* () {
        const document = yield* Workspace.open({
          uri: text.uri,
          bytes: encoder.encode(text.getText()),
        })
        // Overlays read the live document manager, so siblings always see the latest keystroke
        // regardless of which refresh promise settles first.
        const siblings: Array<Document.Document> = []
        for (const open of documents.all()) {
          if (open.uri === text.uri) continue
          siblings.push(
            yield* Workspace.open({ uri: open.uri, bytes: encoder.encode(open.getText()) }),
          )
        }
        const snapshot = yield* Workspace.analyze(document, siblings)
        const moduleUris = new Map<string, string>()
        for (const module of Analysis.modules(snapshot)) {
          const uri = yield* Workspace.uriOf(document.sourceRoot, module.name, siblings)
          if (uri !== undefined) moduleUris.set(module.name, uri)
        }
        if (generations.get(text.uri) !== generation) return
        sessions.set(text.uri, { document, snapshot })
        yield* Effect.promise(() =>
          connection.sendDiagnostics({
            uri: text.uri,
            diagnostics: [
              ...Document.diagnostics(document, snapshot, (module) => moduleUris.get(module)),
            ],
          }),
        )
      }),
    )
  }

  /**
   * A change anywhere refreshes every open document, changed one first: sibling documents see
   * the edit through the overlay resolver, so their diagnostics would go stale otherwise.
   */
  const refreshAll = (changed?: TextDocument): void => {
    const others = documents.all().filter((open) => open.uri !== changed?.uri)
    for (const text of changed === undefined ? others : [changed, ...others]) {
      refresh(text).catch((error) => {
        connection.console.error(`silk-lsp failed to analyze ${text.uri}: ${String(error)}`)
      })
    }
  }

  documents.onDidChangeContent(({ document }) => {
    refreshAll(document)
  })

  documents.onDidClose(({ document }) => {
    sessions.delete(document.uri)
    generations.delete(document.uri)
    void connection.sendDiagnostics({ uri: document.uri, diagnostics: [] })
    // Remaining documents now resolve the closed module from disk instead of the overlay.
    refreshAll()
  })

  connection.onHover((parameters) => {
    const session = sessions.get(parameters.textDocument.uri)
    if (session === undefined) return null
    return Document.hover(session.document, session.snapshot, parameters.position) ?? null
  })

  connection.onDocumentSymbol((parameters) => {
    const session = sessions.get(parameters.textDocument.uri)
    return session === undefined ? [] : [...Document.symbols(session.document, session.snapshot)]
  })

  connection.onDocumentFormatting((parameters) => {
    const session = sessions.get(parameters.textDocument.uri)
    if (session === undefined) return []
    return run(
      Effect.map(Document.format(session.document, session.snapshot), (edits) => [...edits]),
    )
  })

  documents.listen(connection)
  connection.listen()
}
