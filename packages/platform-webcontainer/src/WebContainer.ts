import {
  type BootOptions,
  type BufferEncoding,
  type CodeEvent,
  type CodeEventType,
  type ExportOptions,
  type FileSystemTree,
  type LoadFilesOptions,
  PreviewMessageType,
  type PreviewScriptOptions,
  type PreviewMessage as RawPreviewMessage,
  type WebContainerProcess as RawProcess,
  WebContainer as RawWebContainer,
  type SpawnOptions,
} from '@webcontainer/api'
import * as Context from 'effect/Context'
import * as Effect from 'effect/Effect'
import * as Layer from 'effect/Layer'
import * as Ref from 'effect/Ref'
import type * as Scope from 'effect/Scope'
import type * as Stream from 'effect/Stream'
import * as WebContainerError from './WebContainerError.js'
import * as WebContainerEvent from './WebContainerEvent.js'
import * as WebContainerProcess from './WebContainerProcess.js'

export type {
  BootOptions,
  BufferEncoding,
  CodeEvent,
  CodeEventType,
  ExportOptions,
  FileSystemTree,
  LoadFilesOptions,
  PreviewScriptOptions,
  SpawnOptions,
}

/**
 * Name and entry type returned by the runtime's primitive directory reader.
 *
 * @category models
 * @since 0.0.0
 */
export interface DirectoryEntry {
  readonly name: string
  readonly type: 'File' | 'Directory'
}

/**
 * Raw Node-style watch notification delivered by the runtime's primitive watcher.
 *
 * **Details**
 *
 * `rename` covers both creation and removal, exactly as upstream reports it; classifying which
 * one happened is left to consumers with directory-listing access. Filenames are relative to the
 * watched path and normalized to strings.
 *
 * @category models
 * @since 0.0.0
 */
export interface WatchNotification {
  readonly event: 'rename' | 'change'
  readonly filename: string
}

/**
 * Typed WebContainer filesystem primitives used to build the standard Effect filesystem service.
 *
 * @category services
 * @since 0.0.0
 */
export interface FileSystem {
  readonly makeDirectory: (
    path: string,
    options?: { readonly recursive?: boolean },
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
  readonly readDirectory: (
    path: string,
  ) => Effect.Effect<ReadonlyArray<DirectoryEntry>, WebContainerError.WebContainerError>
  readonly readFile: (
    path: string,
  ) => Effect.Effect<Uint8Array, WebContainerError.WebContainerError>
  readonly readFileString: (
    path: string,
    encoding?: BufferEncoding,
  ) => Effect.Effect<string, WebContainerError.WebContainerError>
  readonly writeFile: (
    path: string,
    data: Uint8Array,
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
  readonly writeFileString: (
    path: string,
    data: string,
    encoding?: BufferEncoding,
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
  readonly remove: (
    path: string,
    options?: { readonly force?: boolean; readonly recursive?: boolean },
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
  readonly rename: (
    oldPath: string,
    newPath: string,
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
  readonly watch: (
    path: string,
    options?: { readonly recursive?: boolean },
  ) => Stream.Stream<WatchNotification, WebContainerError.WebContainerError>
}

/**
 * Safe runtime capabilities exposed by the scoped {@link WebContainer} service.
 *
 * **Details**
 *
 * The service exposes immutable runtime metadata, typed Effects, and lazy event streams. It never
 * exposes the upstream runtime instance, a bare promise, or manual teardown.
 *
 * @category services
 * @since 0.0.0
 */
export interface Service {
  readonly path: string
  readonly workdir: string
  readonly fileSystem: FileSystem
  readonly mount: (
    input: FileSystemTree | Uint8Array | ArrayBuffer,
    options?: LoadFilesOptions,
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
  readonly exportJson: (
    path: string,
    options?: Omit<ExportOptions, 'format'>,
  ) => Effect.Effect<FileSystemTree, WebContainerError.WebContainerError>
  readonly exportBinary: (
    path: string,
    options?: Omit<ExportOptions, 'format'>,
  ) => Effect.Effect<Uint8Array, WebContainerError.WebContainerError>
  readonly exportZip: (
    path: string,
    options?: Omit<ExportOptions, 'format'>,
  ) => Effect.Effect<Uint8Array, WebContainerError.WebContainerError>
  readonly setPreviewScript: (
    scriptSource: string,
    options?: PreviewScriptOptions,
  ) => Effect.Effect<void, WebContainerError.WebContainerError>
  readonly spawn: (
    command: string,
    args?: ReadonlyArray<string>,
    options?: SpawnOptions,
  ) => Effect.Effect<WebContainerProcess.Process, WebContainerError.WebContainerError, Scope.Scope>
  readonly portEvents: Stream.Stream<WebContainerEvent.Port, WebContainerError.WebContainerError>
  readonly serverReadyEvents: Stream.Stream<
    WebContainerEvent.ServerReady,
    WebContainerError.WebContainerError
  >
  readonly internalErrorEvents: Stream.Stream<
    WebContainerEvent.InternalError,
    WebContainerError.WebContainerError
  >
  readonly previewMessageEvents: Stream.Stream<
    WebContainerEvent.PreviewMessage,
    WebContainerError.WebContainerError
  >
}

/**
 * Context service for one scoped in-browser WebContainer runtime.
 *
 * @category services
 * @since 0.0.0
 */
export class WebContainer extends Context.Service<WebContainer, Service>()(
  '@silk-lang/platform-webcontainer/WebContainer',
) {}

/**
 * Safe service and finalizer pair returned by a runtime boot boundary.
 *
 * @category resource management
 * @since 0.0.0
 */
export interface RuntimeHandle {
  readonly service: Service
  readonly teardown: Effect.Effect<void>
}

/**
 * Replaceable Effect-native boot boundary consumed by {@link layerWith}.
 *
 * @category protocols
 * @since 0.0.0
 */
export type Boot = (
  options?: BootOptions,
) => Effect.Effect<RuntimeHandle, WebContainerError.WebContainerError>

const tryExternal = <A>(
  operation: string,
  message: string,
  evaluate: () => A,
  details: WebContainerError.Details = {},
): Effect.Effect<A, WebContainerError.WebContainerError> =>
  Effect.try({
    try: evaluate,
    catch: (cause) => WebContainerError.wrappedFailure(operation, message, cause, details),
  })

const tryExternalPromise = <A>(
  operation: string,
  message: string,
  evaluate: () => PromiseLike<A>,
  details: WebContainerError.Details = {},
): Effect.Effect<A, WebContainerError.WebContainerError> =>
  Effect.tryPromise({
    try: evaluate,
    catch: (cause) => WebContainerError.wrappedFailure(operation, message, cause, details),
  })

const quietFinalizer = (
  operation: string,
  effect: Effect.Effect<void, WebContainerError.WebContainerError>,
) =>
  effect.pipe(
    Effect.catch((error) =>
      Effect.logWarning(`${operation} cleanup failed`).pipe(Effect.annotateLogs('error', error)),
    ),
  )

const makeSubscription = <A>(
  operation: string,
  subscribe: (listener: (event: A) => void) => () => void,
): WebContainerEvent.Subscribe<A> =>
  Effect.fnUntraced(function* (listener) {
    const unsubscribe = yield* tryExternal(operation, `Unable to subscribe to ${operation}`, () =>
      subscribe(listener),
    )
    return {
      unsubscribe: quietFinalizer(
        `${operation}.unsubscribe`,
        tryExternal(
          `${operation}.unsubscribe`,
          `Unable to unsubscribe from ${operation}`,
          unsubscribe,
        ),
      ),
    }
  })

const previewMessage = (message: RawPreviewMessage): WebContainerEvent.PreviewMessage => {
  const location = {
    previewId: message.previewId,
    port: message.port,
    pathname: message.pathname,
    search: message.search,
    hash: message.hash,
  }
  if ('args' in message) {
    return {
      ...location,
      _tag: 'PreviewConsoleError',
      args: message.args,
      stack: message.stack,
    }
  }
  if (message.type === PreviewMessageType.UncaughtException) {
    return {
      ...location,
      _tag: 'PreviewUncaughtException',
      message: message.message,
      stack: message.stack,
    }
  }
  return {
    ...location,
    _tag: 'PreviewUnhandledRejection',
    message: message.message,
    stack: message.stack,
  }
}

const watchDecoder = new TextDecoder()

const makeFileSystem = (raw: RawWebContainer): FileSystem => ({
  makeDirectory: Effect.fnUntraced(function* (path, options) {
    if (options?.recursive === true) {
      yield* tryExternalPromise(
        'WebContainer.fileSystem.makeDirectory',
        `Unable to create directory ${path}`,
        () => raw.fs.mkdir(path, { recursive: true }),
        { path },
      )
    } else {
      yield* tryExternalPromise(
        'WebContainer.fileSystem.makeDirectory',
        `Unable to create directory ${path}`,
        () => raw.fs.mkdir(path),
        { path },
      )
    }
  }),
  readDirectory: Effect.fnUntraced(function* (path) {
    const entries = yield* tryExternalPromise(
      'WebContainer.fileSystem.readDirectory',
      `Unable to read directory ${path}`,
      () => raw.fs.readdir(path, { withFileTypes: true }),
      { path },
    )
    return entries.map((entry) => ({
      name: entry.name,
      type: entry.isDirectory() ? ('Directory' as const) : ('File' as const),
    }))
  }),
  readFile: (path) =>
    tryExternalPromise(
      'WebContainer.fileSystem.readFile',
      `Unable to read file ${path}`,
      () => raw.fs.readFile(path),
      { path },
    ),
  readFileString: (path, encoding = 'utf8') =>
    tryExternalPromise(
      'WebContainer.fileSystem.readFileString',
      `Unable to read file ${path}`,
      () => raw.fs.readFile(path, encoding),
      { path },
    ),
  writeFile: (path, data) =>
    tryExternalPromise(
      'WebContainer.fileSystem.writeFile',
      `Unable to write file ${path}`,
      () => raw.fs.writeFile(path, data),
      { path },
    ),
  writeFileString: (path, data, encoding = 'utf8') =>
    tryExternalPromise(
      'WebContainer.fileSystem.writeFileString',
      `Unable to write file ${path}`,
      () => raw.fs.writeFile(path, data, { encoding }),
      { path },
    ),
  remove: (path, options) =>
    tryExternalPromise(
      'WebContainer.fileSystem.remove',
      `Unable to remove ${path}`,
      () => raw.fs.rm(path, options),
      { path },
    ),
  rename: (oldPath, newPath) =>
    tryExternalPromise(
      'WebContainer.fileSystem.rename',
      `Unable to rename ${oldPath} to ${newPath}`,
      () => raw.fs.rename(oldPath, newPath),
      { path: oldPath },
    ),
  watch: (path, options) =>
    WebContainerEvent.stream(
      makeSubscription('WebContainer.fileSystem.watch', (listener) => {
        const watcher = raw.fs.watch(
          path,
          { recursive: options?.recursive === true },
          (event, filename) =>
            listener({
              event,
              filename: typeof filename === 'string' ? filename : watchDecoder.decode(filename),
            }),
        )
        return () => watcher.close()
      }),
    ),
})

interface ProcessResource {
  readonly raw: RawProcess
  readonly running: Ref.Ref<boolean>
  readonly process: WebContainerProcess.Process
}

const makeProcessResource = Effect.fnUntraced(function* (
  raw: RawProcess,
  command: string,
  outputEnabled: boolean,
): Effect.fn.Return<ProcessResource, never, Scope.Scope> {
  const running = yield* Ref.make(true)
  const exit = yield* Effect.cached(
    tryExternalPromise(
      'WebContainerProcess.exit',
      `Unable to await command ${command}`,
      () => raw.exit,
      { command },
    ).pipe(Effect.onExit(() => Ref.set(running, false))),
  )
  yield* Effect.forkScoped(Effect.ignore(exit))
  const process = WebContainerProcess.fromWebStreams({
    command,
    exit,
    output: outputEnabled ? raw.output : undefined,
    input: raw.input,
    kill: tryExternal(
      'WebContainerProcess.kill',
      `Unable to kill command ${command}`,
      () => raw.kill(),
      { command },
    ),
    resize: (dimensions) =>
      tryExternal(
        'WebContainerProcess.resize',
        `Unable to resize command ${command}`,
        () => raw.resize(dimensions),
        { command },
      ),
  })
  return { raw, running, process }
})

const makeService = (raw: RawWebContainer): Service => ({
  path: raw.path,
  workdir: raw.workdir,
  fileSystem: makeFileSystem(raw),
  mount: (input, options) =>
    tryExternalPromise(
      'WebContainer.mount',
      'Unable to mount filesystem content',
      () => (options === undefined ? raw.mount(input) : raw.mount(input, options)),
      options?.mountPoint === undefined ? {} : { path: options.mountPoint },
    ),
  exportJson: (path, options) =>
    tryExternalPromise(
      'WebContainer.exportJson',
      `Unable to export ${path} as JSON`,
      () => raw.export(path, { ...options, format: 'json' }),
      { path },
    ),
  exportBinary: (path, options) =>
    tryExternalPromise(
      'WebContainer.exportBinary',
      `Unable to export ${path} as a binary snapshot`,
      () => raw.export(path, { ...options, format: 'binary' }),
      { path },
    ),
  exportZip: (path, options) =>
    tryExternalPromise(
      'WebContainer.exportZip',
      `Unable to export ${path} as ZIP`,
      () => raw.export(path, { ...options, format: 'zip' }),
      { path },
    ),
  setPreviewScript: (scriptSource, options) =>
    tryExternalPromise(
      'WebContainer.setPreviewScript',
      'Unable to configure the preview script',
      () =>
        options === undefined
          ? raw.setPreviewScript(scriptSource)
          : raw.setPreviewScript(scriptSource, options),
    ),
  spawn: Effect.fnUntraced(function* (command, args = [], options) {
    const rawProcess = yield* tryExternalPromise(
      'WebContainer.spawn',
      `Unable to spawn command ${command}`,
      () => raw.spawn(command, [...args], options),
      { command },
    )
    return yield* WebContainerProcess.scoped(
      makeProcessResource(rawProcess, command, options?.output !== false).pipe(
        Effect.map(({ process, raw, running }) => ({
          process,
          running: Ref.get(running),
          release: quietFinalizer(
            'WebContainerProcess.release',
            tryExternal(
              'WebContainerProcess.release',
              `Unable to release command ${command}`,
              () => raw.kill(),
              { command },
            ),
          ),
        })),
      ),
    )
  }),
  portEvents: WebContainerEvent.stream(
    makeSubscription('WebContainer.events.port', (listener) =>
      raw.on('port', (port, status, url) => listener(WebContainerEvent.port(port, status, url))),
    ),
  ),
  serverReadyEvents: WebContainerEvent.stream(
    makeSubscription('WebContainer.events.serverReady', (listener) =>
      raw.on('server-ready', (port, url) => listener(WebContainerEvent.serverReady(port, url))),
    ),
  ),
  internalErrorEvents: WebContainerEvent.stream(
    makeSubscription('WebContainer.events.internalError', (listener) =>
      raw.on('error', ({ message }) => listener(WebContainerEvent.internalError(message))),
    ),
  ),
  previewMessageEvents: WebContainerEvent.stream(
    makeSubscription('WebContainer.events.previewMessage', (listener) =>
      raw.on('preview-message', (message) => listener(previewMessage(message))),
    ),
  ),
})

const bootLive: Boot = (options) =>
  tryExternalPromise('WebContainer.boot', 'Unable to boot WebContainer', () =>
    options === undefined ? RawWebContainer.boot() : RawWebContainer.boot(options),
  ).pipe(
    Effect.map((raw) => ({
      service: makeService(raw),
      teardown: quietFinalizer(
        'WebContainer.teardown',
        tryExternal('WebContainer.teardown', 'Unable to tear down WebContainer', () =>
          raw.teardown(),
        ),
      ),
    })),
  )

/**
 * Creates a runtime service value for custom integrations and deterministic tests.
 *
 * @category constructors
 * @since 0.0.0
 */
export const make = (service: Service): Service => service

/**
 * Creates a scoped runtime layer from a replaceable Effect-native boot boundary.
 *
 * **When to use**
 *
 * Use for deterministic tests or alternate acquisition policies that still return a safe
 * {@link RuntimeHandle}. Browser applications normally use {@link layer}.
 *
 * **Gotchas**
 *
 * Constructing the layer is pure; the boot function runs only when the layer is acquired.
 *
 * @category layers
 * @since 0.0.0
 */
export const layerWith = (
  boot: Boot,
  options?: BootOptions,
): Layer.Layer<WebContainer, WebContainerError.WebContainerError> =>
  Layer.effect(
    WebContainer,
    Effect.acquireRelease(
      Effect.suspend(() => boot(options)),
      ({ teardown }) => teardown,
    ).pipe(Effect.map(({ service }) => service)),
  )

/**
 * Creates the live scoped WebContainer layer.
 *
 * **When to use**
 *
 * Use once at a browser application's composition edge, then reuse the same layer value for the
 * runtime service and `WebContainerFileSystem.layer`.
 *
 * **Gotchas**
 *
 * WebContainer permits one active instance. Acquiring independently constructed live layers can
 * create competing boots; share one layer value throughout the graph.
 *
 * **Example** (Sharing one runtime layer)
 *
 * ```ts
 * import * as Layer from 'effect/Layer'
 * import * as WebContainer from '@silk-lang/platform-webcontainer/WebContainer'
 * import * as WebContainerFileSystem from '@silk-lang/platform-webcontainer/WebContainerFileSystem'
 *
 * const runtimeLayer = WebContainer.layer({ workdirName: 'playground' })
 * const fileSystemLayer = WebContainerFileSystem.layer.pipe(Layer.provide(runtimeLayer))
 * const applicationLayer = Layer.merge(runtimeLayer, fileSystemLayer)
 * ```
 *
 * @category layers
 * @since 0.0.0
 */
export const layer = (
  options?: BootOptions,
): Layer.Layer<WebContainer, WebContainerError.WebContainerError> => layerWith(bootLive, options)

/**
 * Provides an already constructed runtime service without acquiring browser resources.
 *
 * @category testing
 * @since 0.0.0
 */
export const layerTest = (service: Service): Layer.Layer<WebContainer> =>
  Layer.succeed(WebContainer)(service)

/**
 * Mounts a filesystem tree or binary snapshot into the current runtime.
 *
 * @category running
 * @since 0.0.0
 */
export const mount = Effect.fn('WebContainer.mount')(function* (
  input: FileSystemTree | Uint8Array | ArrayBuffer,
  options?: LoadFilesOptions,
) {
  const webContainer = yield* WebContainer
  return yield* webContainer.mount(input, options)
})

/**
 * Exports a runtime path as a typed filesystem tree.
 *
 * @category serialization
 * @since 0.0.0
 */
export const exportJson = Effect.fn('WebContainer.exportJson')(function* (
  path: string,
  options?: Omit<ExportOptions, 'format'>,
) {
  const webContainer = yield* WebContainer
  return yield* webContainer.exportJson(path, options)
})

/**
 * Exports a runtime path as WebContainer binary snapshot bytes.
 *
 * @category serialization
 * @since 0.0.0
 */
export const exportBinary = Effect.fn('WebContainer.exportBinary')(function* (
  path: string,
  options?: Omit<ExportOptions, 'format'>,
) {
  const webContainer = yield* WebContainer
  return yield* webContainer.exportBinary(path, options)
})

/**
 * Exports a runtime path as ZIP bytes.
 *
 * @category serialization
 * @since 0.0.0
 */
export const exportZip = Effect.fn('WebContainer.exportZip')(function* (
  path: string,
  options?: Omit<ExportOptions, 'format'>,
) {
  const webContainer = yield* WebContainer
  return yield* webContainer.exportZip(path, options)
})

/**
 * Configures a script injected into future or reloaded runtime previews.
 *
 * @category running
 * @since 0.0.0
 */
export const setPreviewScript = Effect.fn('WebContainer.setPreviewScript')(function* (
  scriptSource: string,
  options?: PreviewScriptOptions,
) {
  const webContainer = yield* WebContainer
  return yield* webContainer.setPreviewScript(scriptSource, options)
})

/**
 * Spawns a scoped WebContainer-native process.
 *
 * **Details**
 *
 * The returned process exposes combined output, terminal input, integer exit, kill, and resize.
 * Closing its scope kills it only while it remains active.
 *
 * @category running
 * @since 0.0.0
 */
export const spawn = Effect.fn('WebContainer.spawn')(function* (
  command: string,
  args?: ReadonlyArray<string>,
  options?: SpawnOptions,
) {
  const webContainer = yield* WebContainer
  return yield* webContainer.spawn(command, args, options)
})
