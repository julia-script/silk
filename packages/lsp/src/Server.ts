import { NodeServices } from '@effect/platform-node'
import * as Cause from 'effect/Cause'
import type * as Duration from 'effect/Duration'
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
import * as DocumentUpdates from './DocumentUpdates.js'
import * as Inspection from './Inspection.js'
import * as ProjectSession from './ProjectSession.js'
import * as Workspace from './Workspace.js'

const encoder = new TextEncoder()

/** Injectable scheduler boundaries used by protocol tests and alternate hosts. */
export interface Options {
  readonly debounce?: Duration.Input
  readonly analyze?: ProjectSession.Options<FileSystem.FileSystem | Path.Path>['analyze']
}

/**
 * The external protocol boundary. `vscode-languageserver` is callback-driven, so this module
 * is the application edge: each handler builds one Effect and runs it against the Node
 * platform services, keeping every module inward of this file effectful and platform-free.
 */
export const start = (options: Options = {}): void => {
  const connection = createConnection(ProposedFeatures.all, process.stdin, process.stdout)
  const documents = new TextDocuments(TextDocument)
  const projects = new Map<string, ProjectSession.ProjectSession>()
  const projectByUri = new Map<string, string>()
  const inFlight = new Set<Promise<unknown>>()
  const documentUpdates = DocumentUpdates.make()
  let shuttingDown = false
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
        signatureHelpProvider: { triggerCharacters: ['(', ','] },
        inlayHintProvider: true,
        documentSymbolProvider: true,
        documentFormattingProvider: true,
        codeActionProvider: {
          codeActionKinds: [CodeActionKind.QuickFix],
          resolveProvider: true,
        },
        semanticTokensProvider: { legend: Document.semanticTokensLegend, full: true },
        foldingRangeProvider: true,
        callHierarchyProvider: true,
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

  const publishProject = Effect.fnUntraced(function* (workspace: string, sourceRoot: string) {
    const project = yield* ProjectSession.make({
      workspace,
      sourceRoot,
      ...(options.debounce === undefined ? {} : { debounce: options.debounce }),
      analyze: options.analyze ?? Workspace.analyzeProject,
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
        // Rides the same publish an analysis commit already makes, so an open inspector view
        // learns it is stale exactly when diagnostics do.
        yield* Effect.promise(() =>
          connection.sendNotification(Inspection.invalidatedNotification, {
            workspace: session.document.workspace,
            uri: session.document.uri,
            version: session.document.version,
          } satisfies Inspection.InvalidatedParameters),
        )
      }),
      failed: Effect.fnUntraced(function* (failure) {
        yield* Effect.sync(() => {
          connection.console.error(
            `silk-lsp project analysis failed for ${failure.document.uri}@${failure.document.version}: ${Cause.pretty(failure.cause)}`,
          )
        })
        yield* Effect.promise(() =>
          connection.sendDiagnostics({
            uri: failure.document.uri,
            version: failure.document.version,
            diagnostics: [],
          }),
        )
      }),
    })
    projects.set(workspace, project)
    return project
  })

  const projectFor = Effect.fnUntraced(function* (workspace: string, sourceRoot: string) {
    const existing = projects.get(workspace)
    return existing ?? (yield* publishProject(workspace, sourceRoot))
  })

  const synchronize = Effect.fnUntraced(function* (text: TextDocument) {
    if (shuttingDown) return
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
    const project = yield* projectFor(document.workspace, document.sourceRoot)
    yield* project.open(document)
  })

  documents.onDidChangeContent(({ document }) => {
    const update = run(
      DocumentUpdates.enqueue(
        documentUpdates,
        document.uri,
        document.version,
        synchronize(document),
      ),
    )
    update.catch((error) => {
      connection.console.error(`silk-lsp failed to analyze ${document.uri}: ${String(error)}`)
    })
  })

  documents.onDidClose(({ document }) => {
    const update = run(
      DocumentUpdates.enqueue(
        documentUpdates,
        document.uri,
        document.version,
        Effect.gen(function* () {
          const workspace = projectByUri.get(document.uri)
          projectByUri.delete(document.uri)
          if (workspace !== undefined) {
            const project = projects.get(workspace)
            if (project !== undefined) {
              yield* project.close(document.uri)
              if (project.documents().length === 0) {
                yield* project.shutdown()
                projects.delete(workspace)
              }
            }
          }
        }),
      ),
    )
    update.catch((error) => {
      connection.console.error(`silk-lsp failed to close ${document.uri}: ${String(error)}`)
    })
    void connection.sendDiagnostics({ uri: document.uri, diagnostics: [] })
  })

  const awaitProjectUpdates = Effect.fnUntraced(function* (project: ProjectSession.ProjectSession) {
    const path = yield* Path.Path
    for (const text of documents.all()) {
      const documentPath = yield* Effect.try({
        try: () => new URL(text.uri),
        catch: (cause) => cause,
      }).pipe(
        Effect.flatMap((url) => path.fromFileUrl(url)),
        Effect.option,
      )
      if (Option.isNone(documentPath)) continue
      const relative = path.relative(project.sourceRoot, documentPath.value)
      if (relative === '..' || relative.startsWith(`..${path.sep}`)) continue
      yield* DocumentUpdates.awaitCurrent(documentUpdates, text.uri)
    }
  })

  const acquire = async (uri: string) => {
    const text = documents.get(uri)
    if (text === undefined) return Option.none<ProjectSession.AnalyzedDocument>()
    const version = text.version
    await run(DocumentUpdates.awaitCurrent(documentUpdates, uri))
    const workspace = projectByUri.get(uri)
    const project = workspace === undefined ? undefined : projects.get(workspace)
    if (project === undefined) return Option.none<ProjectSession.AnalyzedDocument>()
    await run(awaitProjectUpdates(project))
    return await run(project.acquire(uri, version))
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
    return Document.completion(
      session.value.document,
      session.value.snapshot,
      parameters.position,
      session.value.inventory,
    )
  })

  connection.onSignatureHelp(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return null
    return (
      Document.signatureHelp(session.value.document, session.value.snapshot, parameters.position) ??
      null
    )
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
        session.value.inventory,
      ),
    ]
  })

  connection.onCodeActionResolve(async (action) => {
    const data = Document.parseAutoImportData(action.data)
    if (data === undefined)
      return Document.disableCodeAction(action, 'The source action descriptor is invalid')
    await run(DocumentUpdates.awaitCurrent(documentUpdates, data.uri))
    const workspace = projectByUri.get(data.uri)
    const project = workspace === undefined ? undefined : projects.get(workspace)
    if (project === undefined)
      return Document.disableCodeAction(action, 'The source revision is no longer available')
    await run(awaitProjectUpdates(project))
    const session = await run(project.acquire(data.uri, data.version))
    if (Option.isNone(session))
      return Document.disableCodeAction(action, 'The source revision is no longer available')
    return Document.resolveCodeAction(
      session.value.document,
      session.value.snapshot,
      session.value.inventory,
      action,
      (module) => session.value.moduleUris.get(module),
    )
  })

  connection.languages.semanticTokens.on(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return { data: [] }
    return Document.semanticTokens(session.value.document, session.value.snapshot)
  })

  connection.onFoldingRanges(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return []
    return [...Document.foldingRanges(session.value.document, session.value.snapshot)]
  })

  connection.languages.callHierarchy.onPrepare(async (parameters) => {
    const session = await acquire(parameters.textDocument.uri)
    if (Option.isNone(session)) return null
    const items = Document.prepareCallHierarchy(
      session.value.document,
      session.value.snapshot,
      parameters.position,
      (module) => session.value.moduleUris.get(module),
    )
    return items.length === 0 ? null : [...items]
  })

  connection.languages.callHierarchy.onIncomingCalls(async (parameters) => {
    const session = await acquire(parameters.item.uri)
    if (Option.isNone(session)) return null
    return [
      ...Document.incomingCalls(
        session.value.document,
        session.value.snapshot,
        parameters.item,
        (module) => session.value.moduleUris.get(module),
      ),
    ]
  })

  connection.languages.callHierarchy.onOutgoingCalls(async (parameters) => {
    const session = await acquire(parameters.item.uri)
    if (Option.isNone(session)) return null
    return [
      ...Document.outgoingCalls(
        session.value.document,
        session.value.snapshot,
        parameters.item,
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
            for (const project of projects.values()) {
              const relative = path.relative(directory, project.sourceRoot)
              if (relative === '..' || relative.startsWith(`..${path.sep}`)) continue
              yield* project.invalidate({ rediscover: true })
            }
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
            yield* project.invalidate({ dirtyPaths: [changedPath.value] })
          }
        }
      }),
    )
    update.catch((error) => {
      connection.console.error(`silk-lsp failed to process watched files: ${String(error)}`)
    })
  })

  connection.onRequest(Inspection.viewsRequest, () => ({ views: Inspection.descriptors }))

  connection.onRequest(Inspection.viewRequest, async (parameters: Inspection.ViewParameters) => {
    const session = await acquire(parameters.uri)
    if (Option.isNone(session)) {
      return new ResponseError<void>(
        LSPErrorCodes.RequestFailed,
        `No discovered Silk project contains ${parameters.uri}`,
      )
    }
    try {
      const projected = Inspection.project(session.value, parameters)
      return projected._tag === 'UnknownView'
        ? new ResponseError<void>(LSPErrorCodes.RequestFailed, projected.message)
        : projected
    } catch (error) {
      return new ResponseError<void>(
        LSPErrorCodes.RequestFailed,
        `Inspector projection failed: ${error instanceof Error ? error.message : String(error)}`,
      )
    }
  })

  connection.onShutdown(async () => {
    shuttingDown = true
    watcherRegistration?.dispose()
    watcherRegistration = undefined
    await runtime.runPromise(DocumentUpdates.drain(documentUpdates))
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
