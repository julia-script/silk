import { NodeServices } from '@effect/platform-node'
import * as Deferred from 'effect/Deferred'
import * as Effect from 'effect/Effect'
import type * as FileSystem from 'effect/FileSystem'
import * as ManagedRuntime from 'effect/ManagedRuntime'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import * as Queue from 'effect/Queue'
import * as Result from 'effect/Result'
import {
  type CancellationToken,
  CodeActionKind,
  createConnection,
  type DiagnosticServerCancellationData,
  DidChangeWatchedFilesNotification,
  LSPErrorCodes,
  ProposedFeatures,
  ResponseError,
  TextDocumentSyncKind,
  TextDocuments,
} from 'vscode-languageserver/node.js'
import { TextDocument } from 'vscode-languageserver-textdocument'
import * as Document from './Document.js'
import * as DocumentVersion from './DocumentVersion.js'
import type * as EditorQuery from './EditorQuery.js'
import * as Inspection from './Inspection.js'
import * as ProjectWorker from './ProjectWorker.js'
import * as QueryOutcome from './QueryOutcome.js'
import * as SourceEvent from './SourceEvent.js'
import type * as SourceLedger from './SourceLedger.js'
import * as Workspace from './Workspace.js'
import * as WorkspaceEngine from './WorkspaceEngine.js'

const encoder = new TextEncoder()

export const acceptanceRequest = 'silk/acceptedSourceVersions'

export interface AcceptanceRequest {
  readonly entries: ReadonlyArray<WorkspaceEngine.AcceptanceBarrierEntry>
}

export interface AcceptanceResponse {
  readonly accepted: boolean
}

/** Injectable process boundaries used by real-stdio tests and alternate hosts. */
export interface Options {
  readonly policy?: Partial<WorkspaceEngine.Policy>
  readonly discover?: (
    entry: SourceLedger.Entry,
  ) => Effect.Effect<Document.Document, never, FileSystem.FileSystem | Path.Path>
  readonly workerFactory?: ProjectWorker.Factory
}

const emptyCompletion = Object.freeze({ isIncomplete: false, items: [] })
const emptySemanticTokens = Object.freeze({ data: [] })

const ready = <A, B>(outcome: QueryOutcome.QueryOutcome<A>, fallback: B): A | B =>
  outcome._tag === 'Ready' ? outcome.value : fallback

const canceledDiagnostic = (code: number, message: string) =>
  new ResponseError<DiagnosticServerCancellationData>(code, message, { retriggerRequest: true })

const diagnosticFailure = (
  outcome: Exclude<QueryOutcome.QueryOutcome<never>, QueryOutcome.Ready<never>>,
) => {
  switch (outcome._tag) {
    case 'Superseded':
      return canceledDiagnostic(LSPErrorCodes.ServerCancelled, 'Diagnostic revision was superseded')
    case 'Canceled':
      return canceledDiagnostic(LSPErrorCodes.RequestCancelled, 'Diagnostic request was canceled')
    case 'DeadlineExceeded':
      return canceledDiagnostic(
        LSPErrorCodes.ServerCancelled,
        'Diagnostic analysis is still pending',
      )
    case 'AnalysisFailed':
    case 'Unavailable':
    case 'Closed':
      return undefined
  }
}

/**
 * The callback-driven JSON-RPC boundary. Compiler analysis remains behind the workspace engine
 * and its project workers; this module only translates protocol values and never runs Silk code.
 */
export const start = (options: Options = {}): void => {
  const connection = createConnection(ProposedFeatures.all, process.stdin, process.stdout)
  const documents = new TextDocuments(TextDocument)
  const runtime = ManagedRuntime.make(NodeServices.layer)
  let supportsDynamicWatchers = false
  let supportsDiagnosticRefresh = false
  let supportsInlayHintRefresh = false
  let watcherRegistration: { readonly dispose: () => void } | undefined
  let shuttingDown = false
  let profileSettings: unknown

  const discover =
    options.discover ??
    ((entry: SourceLedger.Entry) =>
      Workspace.open({
        uri: entry.uri,
        version: entry.version.value,
        bytes: entry.bytes,
        configuration: profileSettings,
      }))

  const bootstrap = WorkspaceEngine.make({
    discover,
    workerFactory: options.workerFactory ?? ProjectWorker.nodeFactory,
    ...(options.policy === undefined ? {} : { policy: options.policy }),
  })

  void runtime
    .runPromise(bootstrap)
    .then((engine) => {
      const run = <A>(
        effect: Effect.Effect<A, never, FileSystem.FileSystem | Path.Path>,
      ): Promise<A> => runtime.runPromise(effect)

      const request = <K extends EditorQuery.Tag>(
        query: EditorQuery.EditorQuery<K>,
        token: CancellationToken,
      ): Promise<QueryOutcome.QueryOutcome<EditorQuery.Result<K>>> => {
        const canceled = Deferred.makeUnsafe<void>()
        if (token.isCancellationRequested) Deferred.doneUnsafe(canceled, Effect.void)
        const subscription = token.onCancellationRequested(() => {
          Deferred.doneUnsafe(canceled, Effect.void)
        })
        return run(
          Effect.race(
            engine.request(query),
            Deferred.await(canceled).pipe(Effect.as(QueryOutcome.canceled)),
          ),
        ).finally(() => subscription.dispose())
      }

      const accept = (event: SourceEvent.SourceEvent): void => {
        const accepted = engine.accept(event)
        if (Result.isFailure(accepted))
          connection.console.warn(
            `silk-lsp rejected source synchronization for ${accepted.failure.uri}: ${accepted.failure.message}`,
          )
      }

      connection.onInitialize((parameters) => {
        profileSettings = parameters.initializationOptions
        supportsDynamicWatchers =
          parameters.capabilities.workspace?.didChangeWatchedFiles?.dynamicRegistration === true
        supportsDiagnosticRefresh =
          parameters.capabilities.workspace?.diagnostics?.refreshSupport === true
        supportsInlayHintRefresh =
          parameters.capabilities.workspace?.inlayHint?.refreshSupport === true
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
            diagnosticProvider: {
              identifier: 'silk',
              interFileDependencies: true,
              workspaceDiagnostics: false,
            },
          },
        }
      })

      connection.onDidChangeConfiguration((event) => {
        profileSettings = event.settings
        accept(SourceEvent.invalidate([], true))
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
          .catch((error: unknown) => {
            connection.console.error(`silk-lsp failed to register file watchers: ${String(error)}`)
          })
      })

      documents.onDidOpen(({ document }) => {
        accept(
          SourceEvent.open(
            document.uri,
            DocumentVersion.make(document.version),
            encoder.encode(document.getText()),
          ),
        )
      })

      documents.onDidChangeContent(({ document }) => {
        if (engine.accepted([{ uri: document.uri, version: document.version }])) return
        accept(
          SourceEvent.change(
            document.uri,
            DocumentVersion.make(document.version),
            encoder.encode(document.getText()),
          ),
        )
      })

      documents.onDidClose(({ document }) => {
        accept(SourceEvent.close(document.uri))
      })

      connection.onRequest(
        acceptanceRequest,
        (parameters: AcceptanceRequest): AcceptanceResponse => ({
          accepted:
            Array.isArray(parameters.entries) &&
            parameters.entries.every(
              (entry) =>
                typeof entry.uri === 'string' &&
                Number.isSafeInteger(entry.version) &&
                entry.version >= 0,
            ) &&
            engine.accepted(parameters.entries),
        }),
      )

      connection.languages.diagnostics.on(async (parameters, token) => {
        const outcome = await request(
          {
            _tag: 'Diagnostics',
            uri: parameters.textDocument.uri,
            parameters:
              parameters.previousResultId === undefined
                ? {}
                : { previousResultId: parameters.previousResultId },
          },
          token,
        )
        if (outcome._tag === 'Ready')
          return parameters.previousResultId === outcome.value.resultId
            ? { kind: 'unchanged' as const, resultId: outcome.value.resultId }
            : {
                kind: 'full' as const,
                resultId: outcome.value.resultId,
                items: [...outcome.value.items],
              }
        const failure = diagnosticFailure(outcome)
        if (failure !== undefined) return failure
        const version = documents.get(parameters.textDocument.uri)?.version ?? 0
        const resultId = `unavailable:${version}`
        return parameters.previousResultId === resultId
          ? { kind: 'unchanged' as const, resultId }
          : { kind: 'full' as const, resultId, items: [] }
      })

      connection.onHover(
        async (parameters, token) =>
          ready(
            await request(
              { _tag: 'Hover', uri: parameters.textDocument.uri, parameters: parameters.position },
              token,
            ),
            undefined,
          ) ?? null,
      )

      connection.onDefinition(async (parameters, token) => {
        const value = ready(
          await request(
            {
              _tag: 'Definition',
              uri: parameters.textDocument.uri,
              parameters: parameters.position,
            },
            token,
          ),
          undefined,
        )
        return value === undefined ? null : [value]
      })

      connection.onReferences(async (parameters, token) => {
        const locations = ready(
          await request(
            {
              _tag: 'References',
              uri: parameters.textDocument.uri,
              parameters: {
                position: parameters.position,
                includeDeclaration: parameters.context.includeDeclaration,
              },
            },
            token,
          ),
          undefined,
        )
        return locations === undefined ? null : [...locations]
      })

      const unrenameable = (): ResponseError<void> =>
        new ResponseError<void>(
          LSPErrorCodes.RequestFailed,
          'The selected token has no renameable declaration',
        )

      connection.onPrepareRename(async (parameters, token) => {
        const prepared = ready(
          await request(
            {
              _tag: 'PrepareRename',
              uri: parameters.textDocument.uri,
              parameters: parameters.position,
            },
            token,
          ),
          undefined,
        )
        return prepared === undefined
          ? unrenameable()
          : { range: prepared.range, placeholder: prepared.placeholder }
      })

      connection.onRenameRequest(async (parameters, token) => {
        const renamed = ready(
          await request(
            {
              _tag: 'Rename',
              uri: parameters.textDocument.uri,
              parameters: { position: parameters.position, newName: parameters.newName },
            },
            token,
          ),
          undefined,
        )
        if (renamed === undefined) return unrenameable()
        return renamed._tag === 'RenameEdit'
          ? renamed.edit
          : new ResponseError<void>(
              LSPErrorCodes.RequestFailed,
              `${renamed.code}: ${renamed.message}`,
            )
      })

      connection.languages.inlayHint.on(async (parameters, token) => [
        ...ready(
          await request(
            {
              _tag: 'InlayHints',
              uri: parameters.textDocument.uri,
              parameters: parameters.range,
            },
            token,
          ),
          [],
        ),
      ])

      connection.onCompletion(async (parameters, token) =>
        ready(
          await request(
            {
              _tag: 'Completion',
              uri: parameters.textDocument.uri,
              parameters: parameters.position,
            },
            token,
          ),
          emptyCompletion,
        ),
      )

      connection.onSignatureHelp(
        async (parameters, token) =>
          ready(
            await request(
              {
                _tag: 'SignatureHelp',
                uri: parameters.textDocument.uri,
                parameters: parameters.position,
              },
              token,
            ),
            undefined,
          ) ?? null,
      )

      connection.onDocumentSymbol(async (parameters, token) => [
        ...ready(
          await request(
            { _tag: 'Symbols', uri: parameters.textDocument.uri, parameters: undefined },
            token,
          ),
          [],
        ),
      ])

      connection.onCodeAction(async (parameters, token) => [
        ...ready(
          await request(
            {
              _tag: 'CodeActions',
              uri: parameters.textDocument.uri,
              parameters: parameters.range,
            },
            token,
          ),
          [],
        ),
      ])

      connection.onCodeActionResolve(async (action, token) => {
        const data = Document.parseCodeActionData(action.data)
        if (data === undefined)
          return Document.disableCodeAction(action, 'The source action descriptor is invalid')
        return ready(
          await request({ _tag: 'ResolveCodeAction', uri: data.uri, parameters: action }, token),
          Document.disableCodeAction(action, 'The source revision is no longer available'),
        )
      })

      connection.languages.semanticTokens.on(async (parameters, token) =>
        ready(
          await request(
            {
              _tag: 'SemanticTokens',
              uri: parameters.textDocument.uri,
              parameters: undefined,
            },
            token,
          ),
          emptySemanticTokens,
        ),
      )

      connection.onFoldingRanges(async (parameters, token) => [
        ...ready(
          await request(
            {
              _tag: 'FoldingRanges',
              uri: parameters.textDocument.uri,
              parameters: undefined,
            },
            token,
          ),
          [],
        ),
      ])

      connection.languages.callHierarchy.onPrepare(async (parameters, token) => {
        const items = ready(
          await request(
            {
              _tag: 'PrepareCallHierarchy',
              uri: parameters.textDocument.uri,
              parameters: parameters.position,
            },
            token,
          ),
          [],
        )
        return items.length === 0 ? null : [...items]
      })

      connection.languages.callHierarchy.onIncomingCalls(async (parameters, token) => {
        const items = ready(
          await request(
            { _tag: 'IncomingCalls', uri: parameters.item.uri, parameters: parameters.item },
            token,
          ),
          undefined,
        )
        return items === undefined ? null : [...items]
      })

      connection.languages.callHierarchy.onOutgoingCalls(async (parameters, token) => {
        const items = ready(
          await request(
            { _tag: 'OutgoingCalls', uri: parameters.item.uri, parameters: parameters.item },
            token,
          ),
          undefined,
        )
        return items === undefined ? null : [...items]
      })

      connection.onDocumentFormatting(async (parameters, token) => [
        ...ready(
          await request(
            { _tag: 'Formatting', uri: parameters.textDocument.uri, parameters: undefined },
            token,
          ),
          [],
        ),
      ])

      connection.onDidChangeWatchedFiles(({ changes }) => {
        void run(
          Effect.gen(function* () {
            const path = yield* Path.Path
            const paths: Array<string> = []
            let rediscover = false
            for (const change of changes) {
              const parsed = yield* Effect.try(() => new URL(change.uri)).pipe(Effect.option)
              if (Option.isNone(parsed) || parsed.value.protocol !== 'file:') continue
              const changedPath = yield* path.fromFileUrl(parsed.value).pipe(Effect.option)
              if (Option.isNone(changedPath)) continue
              if (path.basename(changedPath.value) === 'silk.toml') rediscover = true
              if (rediscover || changedPath.value.endsWith('.silk')) paths.push(changedPath.value)
            }
            if (paths.length > 0) engine.accept(SourceEvent.invalidate(paths, rediscover))
          }),
        ).catch((error: unknown) => {
          connection.console.error(`silk-lsp failed to process watched files: ${String(error)}`)
        })
      })

      connection.onRequest(Inspection.viewsRequest, () => ({ views: Inspection.descriptors }))

      connection.onRequest(
        Inspection.viewRequest,
        async (parameters: Inspection.ViewParameters, token: CancellationToken) => {
          const projected = ready(
            await request({ _tag: 'Inspection', uri: parameters.uri, parameters }, token),
            undefined,
          )
          if (projected === undefined)
            return new ResponseError<void>(
              LSPErrorCodes.RequestFailed,
              `No discovered Silk project contains ${parameters.uri}`,
            )
          return projected._tag === 'UnknownView'
            ? new ResponseError<void>(LSPErrorCodes.RequestFailed, projected.message)
            : projected
        },
      )

      const observer = Effect.gen(function* () {
        while (!shuttingDown) {
          const event = yield* Queue.take(engine.events)
          switch (event._tag) {
            case 'Committed':
              if (supportsDiagnosticRefresh && event.refreshDiagnostics)
                yield* Effect.sync(() => connection.languages.diagnostics.refresh())
              if (supportsInlayHintRefresh)
                yield* Effect.tryPromise(() => connection.languages.inlayHint.refresh()).pipe(
                  Effect.ignore,
                )
              for (const document of event.documents)
                yield* Effect.tryPromise(() =>
                  connection.sendNotification(Inspection.invalidatedNotification, {
                    workspace: event.workspace,
                    uri: document.uri,
                    version: document.version,
                  } satisfies Inspection.InvalidatedParameters),
                ).pipe(Effect.ignore)
              break
            case 'Failed':
              yield* Effect.sync(() =>
                connection.console.error(
                  `silk-lsp project analysis failed for ${event.workspace} ` +
                    `(generation ${event.generation.value}, incident ${event.incident.value})`,
                ),
              )
              break
            case 'WorkerRetired':
              break
          }
        }
      })
      void run(observer).catch(() => undefined)

      connection.onShutdown(async () => {
        if (shuttingDown) return
        shuttingDown = true
        watcherRegistration?.dispose()
        watcherRegistration = undefined
        await run(engine.shutdown)
        await runtime.dispose()
      })

      documents.listen(connection)
      connection.listen()
    })
    .catch((cause: unknown) => {
      process.stderr.write(`silk-lsp failed to initialize: ${String(cause)}\n`)
      process.exitCode = 1
    })
}
