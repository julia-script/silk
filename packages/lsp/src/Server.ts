import { NodeServices } from '@effect/platform-node'
import * as Effect from 'effect/Effect'
import type * as FileSystem from 'effect/FileSystem'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import {
  CodeActionKind,
  createConnection,
  DidChangeWatchedFilesNotification,
  LSPErrorCodes,
  ProposedFeatures,
  ResponseError,
  TextDocumentSyncKind,
  TextDocuments,
} from 'vscode-languageserver/node.js'
import { TextDocument } from 'vscode-languageserver-textdocument'
import * as Document from './Document.js'
import * as ProjectSession from './ProjectSession.js'
import * as Workspace from './Workspace.js'

const encoder = new TextEncoder()

/**
 * The external protocol boundary. `vscode-languageserver` is callback-driven, so this module
 * is the application edge: each handler builds one Effect and runs it against the Node
 * platform services, keeping every module inward of this file effectful and platform-free.
 */
export const start = (): void => {
  const connection = createConnection(ProposedFeatures.all, process.stdin, process.stdout)
  const documents = new TextDocuments(TextDocument)
  const projects = new Map<
    string,
    ProjectSession.ProjectSession<FileSystem.FileSystem | Path.Path>
  >()
  const projectByUri = new Map<string, string>()
  const inFlight = new Set<Promise<unknown>>()
  const documentUpdates = new Set<Promise<unknown>>()
  let supportsDynamicWatchers = false
  let watcherRegistration: { readonly dispose: () => void } | undefined

  // Platform services are built once for the server's lifetime, not per request.
  const runtime = ManagedRuntime.make(NodeServices.layer)
  const run = <A>(
    effect: Effect.Effect<A, never, FileSystem.FileSystem | Path.Path>,
  ): Promise<A> => {
    const promise = runtime.runPromise(effect)
    inFlight.add(promise)
    promise.finally(() => inFlight.delete(promise)).catch(() => undefined)
    return promise
  }

  connection.onInitialize((parameters) => {
    supportsDynamicWatchers =
      parameters.capabilities.workspace?.didChangeWatchedFiles?.dynamicRegistration === true
    return {
      capabilities: {
        positionEncoding: 'utf-16',
        textDocumentSync: TextDocumentSyncKind.Incremental,
        hoverProvider: true,
        definitionProvider: true,
        referencesProvider: true,
        renameProvider: { prepareProvider: true },
        completionProvider: { triggerCharacters: ['.'] },
        inlayHintProvider: true,
        documentSymbolProvider: true,
        documentFormattingProvider: true,
        codeActionProvider: { codeActionKinds: [CodeActionKind.QuickFix] },
      },
    }
  })

  connection.onInitialized(() => {
    if (!supportsDynamicWatchers) return
    connection.client
      .register(DidChangeWatchedFilesNotification.type, {
        watchers: [{ globPattern: '**/*.silk' }, { globPattern: '**/silk.toml' }],
      })
      .then((registration) => {
        watcherRegistration = registration
      })
      .catch((error) => {
        connection.console.error(`silk-lsp failed to register file watchers: ${String(error)}`)
      })
  })

  const publishProject = (
    workspace: string,
    sourceRoot: string,
  ): ProjectSession.ProjectSession<FileSystem.FileSystem | Path.Path> => {
    const project = ProjectSession.make({
      workspace,
      sourceRoot,
      analyze: Workspace.analyzeProject,
      publish: Effect.fnUntraced(function* (session) {
        yield* Effect.promise(() =>
          connection.sendDiagnostics({
            uri: session.document.uri,
            version: session.document.version,
            diagnostics: [
              ...Document.diagnostics(session.document, session.snapshot, (module) =>
                session.moduleUris.get(module),
              ),
            ],
          }),
        )
      }),
    })
    projects.set(workspace, project)
    return project
  }

  const projectFor = (workspace: string, sourceRoot: string) =>
    projects.get(workspace) ?? publishProject(workspace, sourceRoot)

  const synchronize = Effect.fnUntraced(function* (text: TextDocument) {
    const document = yield* Workspace.open({
      uri: text.uri,
      version: text.version,
      bytes: encoder.encode(text.getText()),
    })
    const previousWorkspace = projectByUri.get(text.uri)
    if (previousWorkspace !== undefined && previousWorkspace !== document.workspace) {
      const previous = projects.get(previousWorkspace)
      if (previous !== undefined) {
        yield* previous.close(text.uri)
        if (previous.documents().length === 0) {
          yield* previous.shutdown()
          projects.delete(previousWorkspace)
        }
      }
    }
    const currentProject = projects.get(document.workspace)
    if (currentProject !== undefined && currentProject.sourceRoot !== document.sourceRoot) {
      yield* currentProject.shutdown()
      projects.delete(document.workspace)
    }
    projectByUri.set(text.uri, document.workspace)
    yield* projectFor(document.workspace, document.sourceRoot).open(document)
  })

  documents.onDidChangeContent(({ document }) => {
    const update = run(synchronize(document))
    documentUpdates.add(update)
    update.finally(() => documentUpdates.delete(update)).catch(() => undefined)
    update.catch((error) => {
      connection.console.error(`silk-lsp failed to analyze ${document.uri}: ${String(error)}`)
    })
  })

  documents.onDidClose(({ document }) => {
    const workspace = projectByUri.get(document.uri)
    projectByUri.delete(document.uri)
    if (workspace !== undefined) {
      const project = projects.get(workspace)
      if (project !== undefined) {
        const update = run(
          Effect.gen(function* () {
            yield* project.close(document.uri)
            if (project.documents().length === 0) {
              yield* project.shutdown()
              projects.delete(workspace)
            }
          }),
        )
        documentUpdates.add(update)
        update.finally(() => documentUpdates.delete(update)).catch(() => undefined)
        update.catch((error) => {
          connection.console.error(`silk-lsp failed to close ${document.uri}: ${String(error)}`)
        })
      }
    }
    void connection.sendDiagnostics({ uri: document.uri, diagnostics: [] })
  })

  const acquire = async (uri: string) => {
    await Promise.all([...documentUpdates])
    const text = documents.get(uri)
    const workspace = projectByUri.get(uri)
    const project = workspace === undefined ? undefined : projects.get(workspace)
    return text === undefined || project === undefined
      ? Option.none<ProjectSession.AnalyzedDocument>()
      : await run(project.acquire(uri, text.version))
  }

  connection.onHover(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return null
    return (
      Document.hover(session.value.document, session.value.snapshot, parameters.position) ?? null
    )
  })

  connection.onDefinition(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return null
    const definition = Document.definition(
      session.value.document,
      session.value.snapshot,
      parameters.position,
      (module) => session.value.moduleUris.get(module),
    )
    return definition === undefined ? null : [definition]
  })

  connection.onReferences(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return null
    const locations = Document.references(
      session.value.document,
      session.value.snapshot,
      parameters.position,
      parameters.context.includeDeclaration,
      (module) => session.value.moduleUris.get(module),
    )
    return locations === undefined ? null : [...locations]
  })

  const unrenameable = (): ResponseError<void> =>
    new ResponseError<void>(
      LSPErrorCodes.RequestFailed,
      'The selected token has no renameable declaration',
    )

  connection.onPrepareRename(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return unrenameable()
    const prepared = Document.prepareRename(
      session.value.document,
      session.value.snapshot,
      parameters.position,
    )
    return prepared === undefined
      ? unrenameable()
      : { range: prepared.range, placeholder: prepared.placeholder }
  })

  connection.onRenameRequest(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return unrenameable()
    const renamed = Document.rename(
      session.value.document,
      session.value.snapshot,
      parameters.position,
      parameters.newName,
      (module) => session.value.moduleUris.get(module),
    )
    if (renamed === undefined) return unrenameable()
    return renamed._tag === 'RenameEdit'
      ? renamed.edit
      : new ResponseError<void>(LSPErrorCodes.RequestFailed, `${renamed.code}: ${renamed.message}`)
  })

  connection.languages.inlayHint.on(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return []
    return [
      ...Document.inlayHints(session.value.document, session.value.snapshot, parameters.range),
    ]
  })

  connection.onCompletion(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return { isIncomplete: false, items: [] }
    return Document.completion(session.value.document, session.value.snapshot, parameters.position)
  })

  connection.onDocumentSymbol(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return []
    return [...Document.symbols(session.value.document, session.value.snapshot)]
  })

  connection.onCodeAction(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return []
    return [
      ...Document.codeActions(
        session.value.document,
        session.value.snapshot,
        parameters.range,
        (module) => session.value.moduleUris.get(module),
      ),
    ]
  })

  connection.onDocumentFormatting(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return []
    return run(
      Effect.map(Document.format(session.value.document, session.value.snapshot), (edits) => [
        ...edits,
      ]),
    )
  })

  connection.onDidChangeWatchedFiles(({ changes }) => {
    const update = run(
      Effect.gen(function* () {
        const path = yield* Path.Path
        for (const change of changes) {
          const parsed = yield* Effect.try({
            try: () => new URL(change.uri),
            catch: (cause) => cause,
          }).pipe(Effect.option)
          if (Option.isNone(parsed) || parsed.value.protocol !== 'file:') continue
          const changedPath = yield* path.fromFileUrl(parsed.value).pipe(Effect.option)
          if (Option.isNone(changedPath)) continue
          const isManifest = path.basename(changedPath.value) === 'silk.toml'
          if (isManifest) {
            const directory = path.dirname(changedPath.value)
            for (const text of documents.all()) {
              const documentPath = yield* Effect.try({
                try: () => new URL(text.uri),
                catch: (cause) => cause,
              }).pipe(
                Effect.flatMap((url) => path.fromFileUrl(url)),
                Effect.option,
              )
              if (Option.isNone(documentPath)) continue
              const relative = path.relative(directory, documentPath.value)
              if (relative === '..' || relative.startsWith(`..${path.sep}`)) continue
              yield* synchronize(text)
            }
            continue
          }
          if (!changedPath.value.endsWith('.silk')) continue
          for (const project of projects.values()) {
            const relative = path.relative(project.sourceRoot, changedPath.value)
            if (relative === '..' || relative.startsWith(`..${path.sep}`)) continue
            yield* project.invalidate()
          }
        }
      }),
    )
    documentUpdates.add(update)
    update.finally(() => documentUpdates.delete(update)).catch(() => undefined)
    update.catch((error) => {
      connection.console.error(`silk-lsp failed to process watched files: ${String(error)}`)
    })
  })

  connection.onShutdown(async () => {
    watcherRegistration?.dispose()
    watcherRegistration = undefined
    await Promise.all(
      [...projects.values()].map((project) => runtime.runPromise(project.shutdown())),
    )
    await Promise.all([...inFlight])
    projects.clear()
    projectByUri.clear()
    await runtime.dispose()
  })

  documents.listen(connection)
  connection.listen()
}
