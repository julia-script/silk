import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import * as PlatformError from 'effect/PlatformError'
import * as Random from 'effect/Random'
import * as Sink from 'effect/Sink'
import * as Stream from 'effect/Stream'
import * as WebContainer from './WebContainer.js'
import * as WebContainerError from './WebContainerError.js'

const moduleName = 'WebContainerFileSystem'

const classify = (error: WebContainerError.WebContainerError): PlatformError.SystemErrorTag => {
  const cause = error.reason._tag === 'WrappedFailure' ? error.reason.cause : error
  const code = WebContainerError.codeOf(cause)
  const message = (WebContainerError.messageOf(cause) ?? error.message).toLowerCase()
  if (code === 'ENOENT' || message.includes('no such file') || message.includes('not found')) {
    return 'NotFound'
  }
  if (code === 'EEXIST' || message.includes('already exists') || message.includes('file exists')) {
    return 'AlreadyExists'
  }
  if (
    code === 'EACCES' ||
    code === 'EPERM' ||
    message.includes('permission denied') ||
    message.includes('not permitted')
  ) {
    return 'PermissionDenied'
  }
  if (
    code === 'EINVAL' ||
    code === 'EISDIR' ||
    code === 'ENOTDIR' ||
    message.includes('invalid') ||
    message.includes('is a directory') ||
    message.includes('not a directory')
  ) {
    return 'InvalidData'
  }
  if (code === 'EBUSY' || message.includes('busy')) {
    return 'Busy'
  }
  return 'Unknown'
}

/**
 * Normalizes a primitive WebContainer filesystem failure into Effect `PlatformError` data.
 *
 * **Details**
 *
 * Guarded codes and messages preserve not-found, already-exists, permission, invalid-data, and
 * busy classifications. Unrecognized failures use `Unknown` and retain their external cause.
 *
 * @category error handling
 * @since 0.0.0
 */
export const platformError = (
  method: string,
  path: string,
  error: WebContainerError.WebContainerError,
): PlatformError.PlatformError => {
  const cause = error.reason._tag === 'WrappedFailure' ? error.reason.cause : error
  return PlatformError.systemError({
    _tag: classify(error),
    module: moduleName,
    method,
    pathOrDescriptor: path,
    description: error.message,
    cause,
  })
}

const unsupported = (method: string, path: string): PlatformError.PlatformError =>
  PlatformError.systemError({
    _tag: 'Unknown',
    module: moduleName,
    method,
    pathOrDescriptor: path,
    description: `${method} is unsupported by WebContainer's virtual filesystem`,
  })

const notFound = (method: string, path: string): PlatformError.PlatformError =>
  PlatformError.systemError({
    _tag: 'NotFound',
    module: moduleName,
    method,
    pathOrDescriptor: path,
    description: `${path} does not exist`,
  })

const alreadyExists = (method: string, path: string): PlatformError.PlatformError =>
  PlatformError.systemError({
    _tag: 'AlreadyExists',
    module: moduleName,
    method,
    pathOrDescriptor: path,
    description: `${path} already exists`,
  })

const invalidData = (method: string, path: string, description: string) =>
  PlatformError.systemError({
    _tag: 'InvalidData',
    module: moduleName,
    method,
    pathOrDescriptor: path,
    description,
  })

const adapt = <A>(
  method: string,
  path: string,
  effect: Effect.Effect<A, WebContainerError.WebContainerError>,
): Effect.Effect<A, PlatformError.PlatformError> =>
  effect.pipe(Effect.mapError((error) => platformError(method, path, error)))

const emptyInfo = (type: FileSystem.File.Type, size: FileSystem.Size): FileSystem.File.Info => ({
  type,
  size,
  mode: 0,
  dev: 0,
  mtime: Option.none(),
  atime: Option.none(),
  birthtime: Option.none(),
  ino: Option.none(),
  nlink: Option.none(),
  uid: Option.none(),
  gid: Option.none(),
  rdev: Option.none(),
  blksize: Option.none(),
  blocks: Option.none(),
})

const encodings: ReadonlyArray<WebContainer.BufferEncoding> = [
  'ascii',
  'utf8',
  'utf-8',
  'utf16le',
  'ucs2',
  'ucs-2',
  'base64',
  'base64url',
  'latin1',
  'binary',
  'hex',
]

const isEncoding = (encoding: string): encoding is WebContainer.BufferEncoding =>
  encodings.some((candidate) => candidate === encoding)

const sizeNumber = (
  method: string,
  path: string,
  input: FileSystem.SizeInput,
): Effect.Effect<number, PlatformError.PlatformError> => {
  const value = BigInt(input)
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    return Effect.fail(
      invalidData(method, path, `${method} requires a nonnegative safe-integer byte size`),
    )
  }
  return Effect.succeed(Number(value))
}

const concatBytes = (chunks: ReadonlyArray<Uint8Array>): Uint8Array => {
  const length = chunks.reduce((total, chunk) => total + chunk.byteLength, 0)
  const output = new Uint8Array(length)
  let offset = 0
  for (const chunk of chunks) {
    output.set(chunk, offset)
    offset += chunk.byteLength
  }
  return output
}

const makeService = Effect.fnUntraced(function* () {
  const runtime = yield* WebContainer.WebContainer
  const path = yield* Path.Path
  const primitive = runtime.fileSystem

  const makeDirectory: FileSystem.FileSystem['makeDirectory'] = Effect.fnUntraced(function* (
    target: string,
    options?: { readonly recursive?: boolean | undefined; readonly mode?: number | undefined },
  ) {
    if (options?.mode !== undefined) {
      return yield* unsupported('makeDirectory.mode', target)
    }
    return yield* adapt(
      'makeDirectory',
      target,
      primitive.makeDirectory(
        target,
        options?.recursive === undefined ? undefined : { recursive: options.recursive },
      ),
    )
  })

  const readFile = (target: string) => adapt('readFile', target, primitive.readFile(target))

  const readFileString = (target: string, encoding = 'utf8') =>
    isEncoding(encoding)
      ? adapt('readFileString', target, primitive.readFileString(target, encoding))
      : Effect.fail(invalidData('readFileString', target, `Unsupported encoding: ${encoding}`))

  const pathExists = Effect.fnUntraced(function* (target: string) {
    const result = yield* Effect.result(stat(target))
    if (result._tag === 'Success') {
      return true
    }
    if (result.failure.reason._tag === 'NotFound') {
      return false
    }
    return yield* result.failure
  })

  const writeBytes: FileSystem.FileSystem['writeFile'] = Effect.fnUntraced(function* (
    target: string,
    data: Uint8Array,
    options?: {
      readonly flag?: FileSystem.OpenFlag | undefined
      readonly mode?: number | undefined
    },
  ) {
    if (options?.mode !== undefined) {
      return yield* unsupported('writeFile.mode', target)
    }
    const flag = options?.flag ?? 'w'
    if (flag === 'r') {
      return yield* invalidData('writeFile', target, 'The r flag is not writable')
    }
    const exists = yield* pathExists(target)
    if ((flag === 'wx' || flag === 'wx+' || flag === 'ax' || flag === 'ax+') && exists) {
      return yield* alreadyExists('writeFile', target)
    }
    if (flag === 'r+' && !exists) {
      return yield* notFound('writeFile', target)
    }
    if (flag === 'a' || flag === 'a+' || flag === 'ax' || flag === 'ax+') {
      const current = exists ? yield* readFile(target) : new Uint8Array(0)
      return yield* adapt(
        'writeFile',
        target,
        primitive.writeFile(target, concatBytes([current, data])),
      )
    }
    return yield* adapt('writeFile', target, primitive.writeFile(target, data))
  })

  const writeString: FileSystem.FileSystem['writeFileString'] = Effect.fnUntraced(function* (
    target: string,
    data: string,
    options?: {
      readonly flag?: FileSystem.OpenFlag | undefined
      readonly mode?: number | undefined
    },
  ) {
    if (options?.mode !== undefined) {
      return yield* unsupported('writeFileString.mode', target)
    }
    const flag = options?.flag ?? 'w'
    if (flag === 'w' || flag === 'w+') {
      return yield* adapt(
        'writeFileString',
        target,
        primitive.writeFileString(target, data, 'utf8'),
      )
    }
    return yield* writeBytes(target, new TextEncoder().encode(data), options)
  })

  const readDirectory: FileSystem.FileSystem['readDirectory'] = Effect.fnUntraced(function* (
    target: string,
    options?: { readonly recursive?: boolean | undefined },
  ) {
    const entries = yield* adapt('readDirectory', target, primitive.readDirectory(target))
    const names = entries.map((entry) => entry.name)
    if (options?.recursive !== true) {
      return names
    }
    const nested = yield* Effect.forEach(entries, (entry) => {
      if (entry.type === 'File') {
        return Effect.succeed([entry.name])
      }
      return readDirectory(path.join(target, entry.name), { recursive: true }).pipe(
        Effect.map((children) => [
          entry.name,
          ...children.map((child) => path.join(entry.name, child)),
        ]),
      )
    })
    return nested.flat()
  })

  const stat = Effect.fnUntraced(function* (target: string) {
    const normalized = path.normalize(target)
    if (normalized === '.' || normalized === '/' || normalized === runtime.workdir) {
      yield* adapt('stat', target, primitive.readDirectory(target))
      return emptyInfo('Directory', FileSystem.Size(0))
    }
    const parent = path.dirname(normalized)
    const name = path.basename(normalized)
    const entries = yield* adapt('stat', target, primitive.readDirectory(parent))
    const entry = entries.find((candidate) => candidate.name === name)
    if (entry === undefined) {
      return yield* notFound('stat', target)
    }
    // File sizes are a documented zero approximation: upstream exposes no stat, and measuring
    // them by reading contents would make every tree walk O(total bytes). Use
    // `readFile(...).byteLength` when an accurate size is required.
    return emptyInfo(entry.type, FileSystem.Size(0))
  })

  const access = Effect.fnUntraced(function* (target: string) {
    yield* stat(target)
  })

  const watchEvent = (
    tag: FileSystem.WatchEvent['_tag'],
    affected: string,
  ): FileSystem.WatchEvent => ({ _tag: tag, path: affected })

  /**
   * Node-style `rename` covers both creation and removal; a parent-listing existence probe
   * (never a content read) decides which one this notification was.
   */
  const renameEvent = Effect.fnUntraced(function* (affected: string) {
    const present = yield* pathExists(affected)
    return watchEvent(present ? 'Create' : 'Remove', affected)
  })

  const watch: FileSystem.FileSystem['watch'] = (target) =>
    Stream.unwrap(
      Effect.gen(function* () {
        const info = yield* stat(target)
        const isDirectory = info.type === 'Directory'
        const base = path.normalize(target)
        return primitive.watch(target, { recursive: isDirectory }).pipe(
          Stream.mapError((error) => platformError('watch', target, error)),
          Stream.mapEffect((notification) => {
            const affected =
              isDirectory && notification.filename !== ''
                ? path.join(base, notification.filename)
                : base
            return notification.event === 'change'
              ? Effect.succeed(watchEvent('Update', affected))
              : renameEvent(affected)
          }),
        )
      }),
    )

  const remove: FileSystem.FileSystem['remove'] = (
    target: string,
    options?: {
      readonly recursive?: boolean | undefined
      readonly force?: boolean | undefined
    },
  ) =>
    adapt(
      'remove',
      target,
      primitive.remove(target, {
        ...(options?.recursive === undefined ? {} : { recursive: options.recursive }),
        ...(options?.force === undefined ? {} : { force: options.force }),
      }),
    )

  const copyFile = Effect.fnUntraced(function* (fromPath: string, toPath: string) {
    const data = yield* readFile(fromPath)
    yield* writeBytes(toPath, data)
  })

  const copy: FileSystem.FileSystem['copy'] = Effect.fnUntraced(function* (
    fromPath: string,
    toPath: string,
    options?: {
      readonly overwrite?: boolean | undefined
      readonly preserveTimestamps?: boolean | undefined
    },
  ) {
    if (options?.preserveTimestamps === true) {
      return yield* unsupported('copy.preserveTimestamps', fromPath)
    }
    const source = path.normalize(fromPath)
    const destination = path.normalize(toPath)
    if (destination.startsWith(`${source}/`)) {
      return yield* invalidData('copy', toPath, 'A directory cannot be copied into itself')
    }
    const destinationExists = yield* pathExists(toPath)
    if (destinationExists && options?.overwrite === false) {
      return yield* alreadyExists('copy', toPath)
    }
    const info = yield* stat(fromPath)
    if (info.type === 'File') {
      return yield* copyFile(fromPath, toPath)
    }
    yield* makeDirectory(toPath, { recursive: true })
    const entries = yield* adapt('copy', fromPath, primitive.readDirectory(fromPath))
    yield* Effect.forEach(
      entries,
      (entry) => copy(path.join(fromPath, entry.name), path.join(toPath, entry.name), options),
      { discard: true },
    )
  })

  const makeTemporary = Effect.fnUntraced(function* (
    kind: 'file' | 'directory',
    options?: {
      readonly directory?: string | undefined
      readonly prefix?: string | undefined
      readonly suffix?: string | undefined
    },
  ) {
    const directory = options?.directory ?? path.join(runtime.workdir, '.tmp')
    yield* makeDirectory(directory, { recursive: true })
    for (let attempt = 0; attempt < 100; attempt += 1) {
      const random = yield* Random.nextInt
      const name = `${options?.prefix ?? ''}${(random >>> 0).toString(36)}${options?.suffix ?? ''}`
      const target = path.join(directory, name)
      if (!(yield* pathExists(target))) {
        if (kind === 'directory') {
          yield* makeDirectory(target)
        } else {
          yield* writeBytes(target, new Uint8Array(0))
        }
        return target
      }
    }
    return yield* PlatformError.systemError({
      _tag: 'AlreadyExists',
      module: moduleName,
      method: kind === 'directory' ? 'makeTempDirectory' : 'makeTempFile',
      pathOrDescriptor: directory,
      description: 'Unable to allocate a unique temporary entry after 100 attempts',
    })
  })

  const makeTempDirectory: FileSystem.FileSystem['makeTempDirectory'] = (options) =>
    makeTemporary('directory', options)

  const makeTempFile: FileSystem.FileSystem['makeTempFile'] = (options) =>
    makeTemporary('file', options)

  const scopedTemporary = (
    acquire: Effect.Effect<string, PlatformError.PlatformError>,
  ): Effect.Effect<string, PlatformError.PlatformError, import('effect/Scope').Scope> =>
    Effect.acquireRelease(acquire, (target) =>
      remove(target, { recursive: true, force: true }).pipe(
        Effect.catch((error) =>
          Effect.logWarning('Unable to remove scoped WebContainer temporary entry').pipe(
            Effect.annotateLogs('error', error),
          ),
        ),
      ),
    )

  const realPath = Effect.fnUntraced(function* (target: string) {
    const resolved = path.isAbsolute(target)
      ? path.normalize(target)
      : path.resolve(runtime.workdir, target)
    yield* stat(resolved)
    return resolved
  })

  const truncate = Effect.fnUntraced(function* (target: string, length: FileSystem.SizeInput = 0) {
    const requested = yield* sizeNumber('truncate', target, length)
    const current = yield* readFile(target)
    if (requested <= current.byteLength) {
      return yield* writeBytes(target, current.slice(0, requested))
    }
    const extended = new Uint8Array(requested)
    extended.set(current)
    return yield* writeBytes(target, extended)
  })

  const fileStream: FileSystem.FileSystem['stream'] = (
    target: string,
    options?: {
      readonly bytesToRead?: FileSystem.SizeInput | undefined
      readonly chunkSize?: FileSystem.SizeInput | undefined
      readonly offset?: FileSystem.SizeInput | undefined
    },
  ): Stream.Stream<Uint8Array, PlatformError.PlatformError> =>
    Stream.unwrap(
      Effect.gen(function* () {
        const bytes = yield* readFile(target)
        const offset = yield* sizeNumber('stream', target, options?.offset ?? 0)
        const chunkSize = yield* sizeNumber(
          'stream',
          target,
          options?.chunkSize ?? FileSystem.KiB(64),
        )
        if (chunkSize === 0) {
          return yield* invalidData('stream', target, 'chunkSize must be greater than zero')
        }
        const available = Math.max(0, bytes.byteLength - offset)
        const limit =
          options?.bytesToRead === undefined
            ? available
            : Math.min(available, yield* sizeNumber('stream', target, options.bytesToRead))
        const chunks: Array<Uint8Array> = []
        for (let index = offset; index < offset + limit; index += chunkSize) {
          chunks.push(bytes.slice(index, Math.min(index + chunkSize, offset + limit)))
        }
        return Stream.fromIterable(chunks)
      }),
    )

  const fileSink: FileSystem.FileSystem['sink'] = (
    target: string,
    options?: {
      readonly flag?: FileSystem.OpenFlag | undefined
      readonly mode?: number | undefined
    },
  ): Sink.Sink<void, Uint8Array, never, PlatformError.PlatformError> =>
    Sink.collect<Uint8Array>().pipe(
      Sink.mapEffect((chunks) => writeBytes(target, concatBytes(chunks), options)),
    )

  const service = FileSystem.FileSystem.of(
    FileSystem.makeNoop({
      access,
      copy,
      copyFile,
      chmod: (target) => Effect.fail(unsupported('chmod', target)),
      chown: (target) => Effect.fail(unsupported('chown', target)),
      glob: (pattern) => Effect.fail(unsupported('glob', pattern)),
      exists: pathExists,
      link: (fromPath) => Effect.fail(unsupported('link', fromPath)),
      makeDirectory,
      makeTempDirectory,
      makeTempDirectoryScoped: (options) => scopedTemporary(makeTempDirectory(options)),
      makeTempFile,
      makeTempFileScoped: (options) => scopedTemporary(makeTempFile(options)),
      open: (target) => Effect.fail(unsupported('open', target)),
      readDirectory,
      readFile,
      readFileString,
      readLink: (target) => Effect.fail(unsupported('readLink', target)),
      realPath,
      remove,
      rename: (oldPath, newPath) => adapt('rename', oldPath, primitive.rename(oldPath, newPath)),
      sink: fileSink,
      stat,
      stream: fileStream,
      symlink: (fromPath) => Effect.fail(unsupported('symlink', fromPath)),
      truncate,
      utimes: (target) => Effect.fail(unsupported('utimes', target)),
      watch,
      writeFile: writeBytes,
      writeFileString: writeString,
    }),
  )
  return service
})

const layerWithPath = Layer.effect(FileSystem.FileSystem, makeService())

/**
 * Provides Effect's standard `FileSystem` service from the current WebContainer runtime.
 *
 * **Details**
 *
 * Native WebContainer operations are wrapped directly. Copy, temporary entries, lexical real
 * paths, truncation, streams, and sinks are derived without random-access handles. Stat metadata is
 * intentionally approximated, and unavailable capabilities fail explicitly.
 *
 * **Gotchas**
 *
 * Streams read a whole file before chunking and sinks buffer every input chunk before one write.
 * Derived operations are not atomic under concurrent filesystem mutation.
 *
 * @category layers
 * @since 0.0.0
 */
export const layer: Layer.Layer<FileSystem.FileSystem, never, WebContainer.WebContainer> =
  layerWithPath.pipe(Layer.provide(Path.layer))
