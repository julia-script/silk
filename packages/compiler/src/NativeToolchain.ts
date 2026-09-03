import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import {
  chmodSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from 'node:fs'
import { arch, platform, tmpdir } from 'node:os'
import { basename, dirname, join, resolve } from 'node:path'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Result from 'effect/Result'
import type * as ArtifactKind from './ArtifactKind.js'
import type * as Backend from './Backend.js'
import * as NativeLinkInput from './NativeLinkInput.js'
import * as Project from './Project.js'
import * as Target from './Target.js'
import * as ToolchainPlan from './ToolchainPlan.js'

export interface Toolchain {
  readonly _tag: 'Toolchain'
  readonly clang: string
  readonly llvmAr: string
  readonly runtimeObjectCache?: RuntimeObjectCache
  readonly artifactCache?: ArtifactCache
}

export interface RuntimeObjectCache {
  readonly _tag: 'RuntimeObjectCache'
  readonly get: (key: string) => Effect.Effect<Uint8Array | undefined, ToolchainError>
  readonly set: (key: string, bytes: Uint8Array) => Effect.Effect<void, ToolchainError>
  readonly stats: () => RuntimeObjectCacheStats
}

export interface RuntimeObjectCacheStats {
  readonly entries: number
  readonly hits: number
  readonly misses: number
}

export const makeRuntimeObjectCache = (): RuntimeObjectCache => {
  const objects = new Map<string, Uint8Array>()
  let hits = 0
  let misses = 0
  return Object.freeze({
    _tag: 'RuntimeObjectCache',
    get: (key: string) =>
      Effect.sync(() => {
        const bytes = objects.get(key)
        if (bytes === undefined) misses += 1
        else hits += 1
        return bytes
      }),
    set: (key: string, bytes: Uint8Array) =>
      Effect.sync(() => {
        objects.set(key, Uint8Array.from(bytes))
      }),
    stats: () => Object.freeze({ entries: objects.size, hits, misses }),
  })
}

export const runtimeObjectCacheStats = (self: RuntimeObjectCache): RuntimeObjectCacheStats =>
  self.stats()

export type Stage =
  | 'host-target'
  | 'cache-key'
  | 'cache-read'
  | 'cache-write'
  | 'scope-acquire'
  | 'scope-cleanup'
  | 'scope-write'
  | 'object'
  | 'runtime'
  | 'link'
  | 'wasm-finalize'
  | 'artifact-commit'
  | 'artifact-cleanup'

export type ToolchainErrorReason =
  | {
      readonly _tag: 'SpawnFailed'
      readonly planned: ToolchainPlan.PlannedCommand
      readonly status: number | null
      readonly output: string
      readonly cause?: unknown
    }
  | {
      readonly _tag: 'StorageFailed'
      readonly path: string
      readonly cause: unknown
    }
  | {
      readonly _tag: 'LinkFailed'
      readonly planned: ToolchainPlan.PlannedCommand
      readonly status: number | null
      readonly output: string
      readonly cause?: unknown
    }
  | {
      readonly _tag: 'UnsupportedPlan'
      readonly plan: ToolchainPlan.UnsupportedNativePlan
    }
  | { readonly _tag: 'InvalidPackageName'; readonly name: string }

/** An expected failure at the Node toolchain boundary. */
export class ToolchainError extends Data.TaggedError('ToolchainError')<{
  readonly operation: string
  readonly stage: Stage
  readonly message: string
  readonly reason: ToolchainErrorReason
}> {}

const storageError = (
  operation: string,
  stage: Stage,
  path: string,
  cause: unknown,
): ToolchainError =>
  new ToolchainError({
    operation,
    stage,
    message: `${operation} failed for ${path}`,
    reason: { _tag: 'StorageFailed', path, cause },
  })

const processError = (
  operation: string,
  stage: 'object' | 'runtime' | 'wasm-finalize',
  planned: ToolchainPlan.PlannedCommand,
  status: number | null,
  output: string,
  cause?: unknown,
): ToolchainError =>
  new ToolchainError({
    operation,
    stage,
    message: `${operation} failed: ${output || `process exited with status ${String(status)}`}`,
    reason: {
      _tag: 'SpawnFailed',
      planned,
      status,
      output,
      ...(cause === undefined ? {} : { cause }),
    },
  })

const linkError = (
  planned: ToolchainPlan.PlannedCommand,
  status: number | null,
  output: string,
  cause?: unknown,
): ToolchainError =>
  new ToolchainError({
    operation: 'NativeToolchain.NativeFinalizer.finalize',
    stage: 'link',
    message: `NativeToolchain.NativeFinalizer.finalize failed: ${output}`,
    reason: {
      _tag: 'LinkFailed',
      planned,
      status,
      output,
      ...(cause === undefined ? {} : { cause }),
    },
  })

const unsupportedPlanError = (plan: ToolchainPlan.UnsupportedNativePlan): ToolchainError =>
  new ToolchainError({
    operation: 'NativeToolchain.NativeFinalizer.finalize',
    stage: 'link',
    message: `NativeToolchain.NativeFinalizer.finalize cannot preserve ${plan.reason} for ${plan.artifactKind} on ${plan.target.id}`,
    reason: { _tag: 'UnsupportedPlan', plan },
  })

const invalidPackageNameError = (name: string): ToolchainError =>
  new ToolchainError({
    operation: 'NativeToolchain.commitLibraryInterface',
    stage: 'artifact-commit',
    message: `NativeToolchain.commitLibraryInterface requires a portable package name, received ${name}`,
    reason: { _tag: 'InvalidPackageName', name },
  })

export interface ArtifactCache {
  readonly _tag: 'ArtifactCache'
  readonly get: (key: string) => Effect.Effect<Uint8Array | undefined, ToolchainError>
  readonly set: (key: string, bytes: Uint8Array) => Effect.Effect<void, ToolchainError>
}

const artifactCacheMagic = Uint8Array.from([0x53, 0x49, 0x4c, 0x4b, 0x43, 0x30, 0x30, 0x31])
const artifactCacheDigestLength = 32
const artifactCacheHeaderLength = artifactCacheMagic.length + 4 + artifactCacheDigestLength

const artifactCacheDigest = (key: string, bytes: Uint8Array): Uint8Array =>
  createHash('sha256').update(key).update('\0').update(bytes).digest()

const encodeArtifactCacheEntry = (key: string, bytes: Uint8Array): Uint8Array => {
  if (bytes.length > 0xffff_ffff) throw new RangeError('artifact cache entry exceeds 4 GiB')
  const encoded = new Uint8Array(artifactCacheHeaderLength + bytes.length)
  encoded.set(artifactCacheMagic)
  const lengthOffset = artifactCacheMagic.length
  encoded[lengthOffset] = bytes.length & 0xff
  encoded[lengthOffset + 1] = (bytes.length >>> 8) & 0xff
  encoded[lengthOffset + 2] = (bytes.length >>> 16) & 0xff
  encoded[lengthOffset + 3] = (bytes.length >>> 24) & 0xff
  encoded.set(artifactCacheDigest(key, bytes), lengthOffset + 4)
  encoded.set(bytes, artifactCacheHeaderLength)
  return encoded
}

const decodeArtifactCacheEntry = (key: string, encoded: Uint8Array): Uint8Array | undefined => {
  if (encoded.length < artifactCacheHeaderLength) return undefined
  if (!artifactCacheMagic.every((byte, index) => encoded[index] === byte)) return undefined
  const lengthOffset = artifactCacheMagic.length
  const payloadLength =
    (encoded[lengthOffset] ?? 0) +
    (encoded[lengthOffset + 1] ?? 0) * 0x100 +
    (encoded[lengthOffset + 2] ?? 0) * 0x1_0000 +
    (encoded[lengthOffset + 3] ?? 0) * 0x100_0000
  if (artifactCacheHeaderLength + payloadLength !== encoded.length) return undefined
  const payload = encoded.subarray(artifactCacheHeaderLength)
  const expectedDigest = encoded.subarray(
    lengthOffset + 4,
    lengthOffset + 4 + artifactCacheDigestLength,
  )
  const actualDigest = artifactCacheDigest(key, payload)
  if (!actualDigest.every((byte, index) => expectedDigest[index] === byte)) return undefined
  return Uint8Array.from(payload)
}

/** Reads a caller-supplied artifact cache without allowing callback throws past the boundary. */
export const readArtifactCache = Effect.fnUntraced(function* (
  cache: ArtifactCache,
  key: string,
): Effect.fn.Return<Uint8Array | undefined, ToolchainError> {
  const operation = yield* Effect.try({
    try: () => cache.get(key),
    catch: (cause) => storageError('NativeToolchain.ArtifactCache.get', 'cache-read', key, cause),
  })
  const encoded = yield* operation.pipe(
    Effect.mapError((cause) =>
      cause instanceof ToolchainError
        ? cause
        : storageError('NativeToolchain.ArtifactCache.get', 'cache-read', key, cause),
    ),
  )
  if (encoded === undefined) return undefined
  return yield* Effect.try({
    try: () => decodeArtifactCacheEntry(key, encoded),
    catch: (cause) => storageError('NativeToolchain.ArtifactCache.get', 'cache-read', key, cause),
  })
})

/** Writes a caller-supplied artifact cache without allowing callback throws past the boundary. */
export const writeArtifactCache = Effect.fnUntraced(function* (
  cache: ArtifactCache,
  key: string,
  bytes: Uint8Array,
): Effect.fn.Return<void, ToolchainError> {
  const encoded = yield* Effect.try({
    try: () => encodeArtifactCacheEntry(key, bytes),
    catch: (cause) => storageError('NativeToolchain.ArtifactCache.set', 'cache-write', key, cause),
  })
  const operation = yield* Effect.try({
    try: () => cache.set(key, encoded),
    catch: (cause) => storageError('NativeToolchain.ArtifactCache.set', 'cache-write', key, cause),
  })
  return yield* operation.pipe(
    Effect.mapError((cause) =>
      cause instanceof ToolchainError
        ? cause
        : storageError('NativeToolchain.ArtifactCache.set', 'cache-write', key, cause),
    ),
  )
})

let atomicNonce = 0

export interface CleanupOperation {
  readonly remove: (
    path: string,
    options: { readonly force: true; readonly recursive?: boolean },
  ) => void
}

class CleanupAttemptError extends Data.TaggedError('CleanupAttemptError')<{
  readonly cause: unknown
}> {}

const nodeCleanup: CleanupOperation = Object.freeze({
  remove: (path: string, options: { readonly force: true; readonly recursive?: boolean }) =>
    rmSync(path, options),
})

/** Removes one scoped path, retrying once while retaining both arbitrary failure causes. */
const cleanupPath = Effect.fnUntraced(function* (
  path: string,
  options: { readonly force: true; readonly recursive?: boolean },
  cleanup: CleanupOperation,
  stage: 'scope-cleanup' | 'artifact-cleanup',
): Effect.fn.Return<void, ToolchainError> {
  const first = yield* Effect.result(
    Effect.try({
      try: () => cleanup.remove(path, options),
      catch: (cause) => new CleanupAttemptError({ cause }),
    }),
  )
  if (Result.isSuccess(first)) return
  const retry = yield* Effect.result(
    Effect.try({
      try: () => cleanup.remove(path, options),
      catch: (cause) => new CleanupAttemptError({ cause }),
    }),
  )
  if (Result.isSuccess(retry)) return
  const fallback = yield* Effect.result(
    Effect.try({
      try: () => nodeCleanup.remove(path, options),
      catch: (cause) => new CleanupAttemptError({ cause }),
    }),
  )
  return yield* storageError('NativeToolchain.cleanupPath', stage, path, {
    first: first.failure.cause,
    retry: retry.failure.cause,
    ...(Result.isFailure(fallback) ? { fallback: fallback.failure.cause } : {}),
  })
})

const releaseCleanup = <A, E>(
  exit: Exit.Exit<A, E>,
  cleanup: Effect.Effect<void, ToolchainError>,
): Effect.Effect<void, ToolchainError> =>
  Exit.isSuccess(exit)
    ? cleanup
    : cleanup.pipe(
        Effect.catch((error) =>
          Effect.logError(error).pipe(
            Effect.annotateLogs('cleanupOperation', error.operation),
            Effect.annotateLogs('cleanupStage', error.stage),
          ),
        ),
      )

/** Atomically writes bytes through a unique same-directory temporary sibling. */
export const atomicCommit = Effect.fn('NativeToolchain.atomicCommit')(function* (
  destination: string,
  bytes: Uint8Array,
  options: {
    readonly mode?: number
    readonly stage?: Stage
    readonly cleanup?: CleanupOperation
  } = {},
): Effect.fn.Return<string, ToolchainError> {
  const path = resolve(destination)
  const temporary = `${path}.silk-tmp-${process.pid}-${atomicNonce++}`
  const stage = options.stage ?? 'artifact-commit'
  return yield* Effect.acquireUseRelease(
    Effect.try({
      try: () => {
        mkdirSync(dirname(path), { recursive: true })
        return temporary
      },
      catch: (cause) => storageError('NativeToolchain.atomicCommit.prepare', stage, path, cause),
    }),
    (staged) =>
      Effect.try({
        try: () => {
          writeFileSync(
            staged,
            bytes,
            options.mode === undefined ? undefined : { mode: options.mode },
          )
          renameSync(staged, path)
          if (options.mode !== undefined) chmodSync(path, options.mode)
          return path
        },
        catch: (cause) => storageError('NativeToolchain.atomicCommit', stage, path, cause),
      }),
    (staged, exit) =>
      releaseCleanup(
        exit,
        cleanupPath(staged, { force: true }, options.cleanup ?? nodeCleanup, 'artifact-cleanup'),
      ),
  )
})

const cleanupCommittedOnFailure = <A, E>(
  path: string,
  exit: Exit.Exit<A, E>,
): Effect.Effect<void, ToolchainError> =>
  Exit.isFailure(exit)
    ? releaseCleanup(exit, cleanupPath(path, { force: true }, nodeCleanup, 'artifact-cleanup'))
    : Effect.void

const cleanupCommittedSetOnFailure = <A, E>(
  paths: ReadonlyArray<string>,
  exit: Exit.Exit<A, E>,
): Effect.Effect<void, ToolchainError> =>
  Exit.isFailure(exit)
    ? Effect.forEach(
        paths,
        (path) =>
          releaseCleanup(exit, cleanupPath(path, { force: true }, nodeCleanup, 'artifact-cleanup')),
        { discard: true },
      )
    : Effect.void

/** Commits a native library's header and ABI manifest, removing the whole set on failure. */
export const commitLibraryInterface = Effect.fn('NativeToolchain.commitLibraryInterface')(
  function* (
    artifactPath: string,
    destination: string,
    packageName: string,
    cHeader: Uint8Array,
    abiManifest: Uint8Array,
  ): Effect.fn.Return<LibraryInterfaceArtifacts, ToolchainError> {
    const directory = dirname(resolve(destination))
    const cHeaderDestination = join(directory, `${packageName}.h`)
    const abiManifestDestination = join(directory, `${packageName}.abi.json`)
    return yield* Effect.acquireUseRelease(
      Effect.succeed(resolve(artifactPath)),
      () =>
        Effect.gen(function* () {
          if (!Project.isPackageName(packageName))
            return yield* invalidPackageNameError(packageName)
          return yield* Effect.acquireUseRelease(
            Effect.succeed(Object.freeze([cHeaderDestination, abiManifestDestination])),
            () =>
              Effect.gen(function* () {
                const cHeaderPath = yield* atomicCommit(cHeaderDestination, cHeader)
                const abiManifestPath = yield* atomicCommit(abiManifestDestination, abiManifest)
                return Object.freeze({
                  _tag: 'LibraryInterfaceArtifacts' as const,
                  cHeader: cHeaderPath,
                  abiManifest: abiManifestPath,
                })
              }),
            cleanupCommittedSetOnFailure,
          )
        }),
      cleanupCommittedOnFailure,
    )
  },
)

export const makeDiskArtifactCache = (directory: string): ArtifactCache => {
  const root = resolve(directory)
  return Object.freeze({
    _tag: 'ArtifactCache',
    get: Effect.fnUntraced(function* (key: string) {
      const path = join(root, key)
      return yield* Effect.try({
        try: () => (existsSync(path) ? readFileSync(path) : undefined),
        catch: (cause) =>
          storageError('NativeToolchain.ArtifactCache.get', 'cache-read', path, cause),
      })
    }),
    set: Effect.fnUntraced(function* (key: string, bytes: Uint8Array) {
      yield* atomicCommit(join(root, key), bytes, { stage: 'cache-write' })
    }),
  })
}

const processArtifactCache = new Map<string, Uint8Array>()

export const defaultArtifactCache = (directory = ''): ArtifactCache => {
  if (directory !== '') return makeDiskArtifactCache(directory)
  return Object.freeze({
    _tag: 'ArtifactCache',
    get: (key: string) => Effect.succeed(processArtifactCache.get(key)),
    set: (key: string, bytes: Uint8Array) =>
      Effect.sync(() => {
        processArtifactCache.set(key, Uint8Array.from(bytes))
      }),
  })
}

const artifactExtension = (kind: FinalArtifact['kind'], target: Target.Target): string => {
  if (kind === 'NativeExecutable') return 'bin'
  if (kind === 'NativeSharedLibrary') return target.id === 'aarch64-apple-darwin' ? 'dylib' : 'so'
  if (kind === 'NativeStaticLibrary') return 'a'
  return 'wasm'
}

const toolVersions = new Map<string, string>()

const toolVersionOf = Effect.fnUntraced(function* (
  command: string,
): Effect.fn.Return<string, ToolchainError> {
  const cached = toolVersions.get(command)
  if (cached !== undefined) return cached
  const planned: ToolchainPlan.PlannedCommand = Object.freeze({
    _tag: 'PlannedCommand',
    command,
    arguments: Object.freeze(['--version']),
    target: Target.wasm32UnknownUnknown,
  })
  const result = yield* Effect.try({
    try: () => spawnSync(command, ['--version'], { encoding: 'utf8' }),
    catch: (cause) =>
      processError('NativeToolchain.toolVersionOf', 'object', planned, null, '', cause),
  })
  if (result.error !== undefined || result.status !== 0) {
    return yield* processError(
      'NativeToolchain.toolVersionOf',
      'object',
      planned,
      result.status,
      `${result.stdout ?? ''}${result.stderr ?? ''}${result.error?.message ?? ''}`,
      result.error,
    )
  }
  const version = result.stdout.split('\n', 1)[0] ?? ''
  toolVersions.set(command, version)
  return version
})

export const artifactCacheKey = Effect.fn('NativeToolchain.artifactCacheKey')(function* (
  toolchain: Toolchain,
  kind: FinalArtifact['kind'],
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
  bitcode: Uint8Array | string,
  runtimeSource: string,
  destination: string,
  nativeLinkInputs: ReadonlyArray<NativeLinkInput.NativeLinkInput> = Object.freeze([]),
): Effect.fn.Return<string, ToolchainError> {
  const clangVersion = yield* toolVersionOf(toolchain.clang)
  const archiveCommand = kind === 'NativeStaticLibrary' ? toolchain.llvmAr : ''
  const llvmArVersion = kind === 'NativeStaticLibrary' ? yield* toolVersionOf(archiveCommand) : ''
  const embeddedArtifactName =
    kind === 'NativeSharedLibrary' && target.id === 'aarch64-apple-darwin'
      ? basename(destination)
      : ''
  const inputBytes = new Map<number, Uint8Array>()
  for (const [index, input] of nativeLinkInputs.entries()) {
    const path = NativeLinkInput.path(input)
    if (path !== undefined) {
      inputBytes.set(
        index,
        yield* Effect.try({
          try: () => readFileSync(path),
          catch: (cause) =>
            storageError('NativeToolchain.artifactCacheKey', 'cache-key', path, cause),
        }),
      )
    }
  }
  return yield* Effect.try({
    try: () => {
      const digest = createHash('sha256')
      for (const value of [
        kind,
        target.id,
        profile,
        toolchain.clang,
        clangVersion,
        archiveCommand,
        llvmArVersion,
        runtimeSource,
        embeddedArtifactName,
      ]) {
        digest.update(value)
        digest.update('\0')
      }
      digest.update(bitcode)
      for (const [index, input] of nativeLinkInputs.entries()) {
        digest.update(`\0input:${NativeLinkInput.encode(input)}\0`)
        const bytes = inputBytes.get(index)
        if (bytes !== undefined) {
          digest.update(`bytes:${bytes.length}\0`)
          digest.update(bytes)
        }
      }
      return `${digest.digest('hex')}.${artifactExtension(kind, target)}`
    },
    catch: (cause) =>
      storageError('NativeToolchain.artifactCacheKey', 'cache-key', toolchain.clang, cause),
  })
})

export interface PathArtifact {
  readonly _tag: 'PathArtifact'
  readonly scope: string
  readonly path: string
  readonly target: Target.Target
}

export interface BuildScope {
  readonly _tag: 'BuildScope'
  readonly name: string
  readonly root: string
}

export interface ObjectArtifact {
  readonly _tag: 'ObjectArtifact'
  readonly artifact: PathArtifact
  readonly planned: ToolchainPlan.PlannedCommand
}

export interface FinalArtifact {
  readonly _tag: 'FinalArtifact'
  readonly kind: ArtifactKind.ArtifactKind
  readonly path: string
  readonly bytes: Uint8Array
  readonly target: Target.Target
  readonly planned?: ToolchainPlan.PlannedCommand
}

/** Durable C-consumer companions committed beside one native library. */
export interface LibraryInterfaceArtifacts {
  readonly _tag: 'LibraryInterfaceArtifacts'
  /** The durable generated C-header path. */
  readonly cHeader: string
  /** The durable generated ABI-manifest path. */
  readonly abiManifest: string
}

export const hostTarget = Effect.fn('NativeToolchain.hostTarget')(function* (): Effect.fn.Return<
  Target.Target,
  Target.TargetError
> {
  return yield* Target.fromHost(platform(), arch())
})

/** Selects the current Node process host without pulling Node detection into the pure target actor. */
export const hostSelection = (): Target.Selection => Target.select(undefined, platform(), arch())

export const withBuildScope = Effect.fn('NativeToolchain.withBuildScope')(function* <A, E, R>(
  name: string,
  run: (scope: BuildScope) => Effect.Effect<A, E, R>,
  options: { readonly saveTemps?: boolean; readonly cleanup?: CleanupOperation } = {},
): Effect.fn.Return<A, E | ToolchainError, R> {
  return yield* Effect.acquireUseRelease(
    Effect.try({
      try: () => {
        const root = mkdtempSync(join(tmpdir(), `silk-${name.replace(/[^A-Za-z0-9_-]/g, '_')}-`))
        return Object.freeze({ _tag: 'BuildScope' as const, name, root })
      },
      catch: (cause) =>
        storageError('NativeToolchain.withBuildScope', 'scope-acquire', tmpdir(), cause),
    }),
    run,
    (scope, exit) =>
      options.saveTemps === true
        ? Effect.void
        : releaseCleanup(
            exit,
            cleanupPath(
              scope.root,
              { recursive: true, force: true },
              options.cleanup ?? nodeCleanup,
              'scope-cleanup',
            ),
          ),
  )
})

const readRuntimeObjectCache = Effect.fnUntraced(function* (
  cache: RuntimeObjectCache,
  key: string,
): Effect.fn.Return<Uint8Array | undefined, ToolchainError> {
  const operation = yield* Effect.try({
    try: () => cache.get(key),
    catch: (cause) =>
      storageError('NativeToolchain.RuntimeObjectCache.get', 'cache-read', key, cause),
  })
  return yield* operation.pipe(
    Effect.mapError((cause) =>
      storageError('NativeToolchain.RuntimeObjectCache.get', 'cache-read', key, cause),
    ),
  )
})

const writeRuntimeObjectCache = Effect.fnUntraced(function* (
  cache: RuntimeObjectCache,
  key: string,
  bytes: Uint8Array,
): Effect.fn.Return<void, ToolchainError> {
  const operation = yield* Effect.try({
    try: () => cache.set(key, bytes),
    catch: (cause) =>
      storageError('NativeToolchain.RuntimeObjectCache.set', 'cache-write', key, cause),
  })
  return yield* operation.pipe(
    Effect.mapError((cause) =>
      storageError('NativeToolchain.RuntimeObjectCache.set', 'cache-write', key, cause),
    ),
  )
})

export const writeArtifact = Effect.fn('NativeToolchain.writeArtifact')(function* (
  scope: BuildScope,
  target: Target.Target,
  fileName: string,
  bytes: Uint8Array | string,
): Effect.fn.Return<PathArtifact, ToolchainError> {
  const path = join(scope.root, fileName)
  yield* Effect.try({
    try: () => writeFileSync(path, bytes),
    catch: (cause) => storageError('NativeToolchain.writeArtifact', 'scope-write', path, cause),
  })
  return Object.freeze({ _tag: 'PathArtifact', scope: scope.name, path, target })
})

const runPlanned = Effect.fnUntraced(function* (
  operation: string,
  stage: 'object' | 'runtime' | 'wasm-finalize',
  planned: ToolchainPlan.PlannedCommand,
): Effect.fn.Return<void, ToolchainError> {
  const result = yield* Effect.try({
    try: () => spawnSync(planned.command, [...planned.arguments], { encoding: 'utf8' }),
    catch: (cause) => processError(operation, stage, planned, null, '', cause),
  })
  const output = `${result.stdout ?? ''}${result.stderr ?? ''}${result.error?.message ?? ''}`
  if (result.error !== undefined || result.status !== 0) {
    return yield* processError(operation, stage, planned, result.status, output, result.error)
  }
})

const requirePath = Effect.fnUntraced(function* (
  operation: string,
  stage: Stage,
  path: string,
): Effect.fn.Return<void, ToolchainError> {
  const exists = yield* Effect.try({
    try: () => existsSync(path),
    catch: (cause) => storageError(operation, stage, path, cause),
  })
  if (!exists)
    return yield* storageError(operation, stage, path, new Error('expected output is missing'))
})

export const emitObject = Effect.fn('NativeToolchain.emitObject')(function* (
  toolchain: Toolchain,
  scope: BuildScope,
  artifact: Backend.LlvmBitcodeArtifact,
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
  baseName = 'program',
): Effect.fn.Return<ObjectArtifact, ToolchainError> {
  const bitcodePath = join(scope.root, `${baseName}.bc`)
  const objectPath = join(scope.root, `${baseName}.o`)
  const planned = ToolchainPlan.objectCommand(
    toolchain.clang,
    target,
    profile,
    bitcodePath,
    objectPath,
  )
  if (artifact.target.id !== target.id) {
    return yield* processError(
      'NativeToolchain.emitObject',
      'object',
      planned,
      null,
      `bitcode target ${artifact.target.id} does not match requested target ${target.id}`,
    )
  }
  yield* writeArtifact(scope, target, `${baseName}.bc`, artifact.bitcode)
  yield* runPlanned('NativeToolchain.emitObject', 'object', planned)
  yield* requirePath('NativeToolchain.emitObject', 'object', objectPath)
  return Object.freeze({
    _tag: 'ObjectArtifact',
    artifact: Object.freeze({
      _tag: 'PathArtifact',
      scope: scope.name,
      path: objectPath,
      target,
    }),
    planned,
  })
})

/**
 * Compiles one C translation unit to `<scope>/<name>.o` through the pinned Clang `-c -x c`
 * command, reusing the runtime-object cache keyed by Clang, target, and source text.
 */
export const compileCObject = Effect.fn('NativeToolchain.compileCObject')(function* (
  toolchain: Toolchain,
  scope: BuildScope,
  target: Target.Target,
  name: string,
  sourceText: string,
): Effect.fn.Return<ObjectArtifact, ToolchainError> {
  const cacheKey = `${toolchain.clang}\u0000${target.id}\u0000${sourceText}`
  const objectPath = join(scope.root, `${name}.o`)
  const source = yield* writeArtifact(scope, target, `${name}.c`, sourceText)
  const planned = ToolchainPlan.cObjectCommand(toolchain.clang, target, source.path, objectPath)
  const cached =
    toolchain.runtimeObjectCache === undefined
      ? undefined
      : yield* readRuntimeObjectCache(toolchain.runtimeObjectCache, cacheKey)
  if (cached !== undefined) {
    yield* writeArtifact(scope, target, `${name}.o`, cached)
  } else {
    yield* runPlanned('NativeToolchain.compileCObject', 'runtime', planned)
    yield* requirePath('NativeToolchain.compileCObject', 'runtime', objectPath)
    const bytes = yield* Effect.try({
      try: () => readFileSync(objectPath),
      catch: (cause) =>
        storageError('NativeToolchain.compileCObject', 'runtime', objectPath, cause),
    })
    if (toolchain.runtimeObjectCache !== undefined)
      yield* writeRuntimeObjectCache(toolchain.runtimeObjectCache, cacheKey, bytes)
  }
  return Object.freeze({
    _tag: 'ObjectArtifact',
    artifact: Object.freeze({
      _tag: 'PathArtifact',
      scope: scope.name,
      path: objectPath,
      target,
    }),
    planned,
  })
})

export const compileExecutableRuntime = Effect.fn('NativeToolchain.compileExecutableRuntime')(
  function* (
    toolchain: Toolchain,
    scope: BuildScope,
    target: Target.Target,
    termination: Backend.Termination,
    nativeRuntimeSymbols: ReadonlyArray<string> = Object.freeze([]),
  ): Effect.fn.Return<ObjectArtifact, ToolchainError> {
    return yield* compileCObject(
      toolchain,
      scope,
      target,
      'silk_runtime',
      ToolchainPlan.executableSource(termination, nativeRuntimeSymbols),
    )
  },
)

export const compileRuntime = Effect.fn('NativeToolchain.compileRuntime')(function* (
  toolchain: Toolchain,
  scope: BuildScope,
  target: Target.Target,
  nativeRuntimeSymbols: ReadonlyArray<string> = Object.freeze([]),
): Effect.fn.Return<ObjectArtifact, ToolchainError> {
  return yield* compileCObject(
    toolchain,
    scope,
    target,
    'silk_runtime',
    ToolchainPlan.runtimeSource(nativeRuntimeSymbols),
  )
})

/**
 * Plans the link and rejects an input that targets another triple or is missing on disk, so a
 * bad object is reported as linker input data wherever the check runs.
 */
export const requireLinkInputs = Effect.fnUntraced(function* (
  toolchain: Toolchain,
  artifactKind: ToolchainPlan.NativeArtifactKind,
  target: Target.Target,
  generatedObjects: ReadonlyArray<PathArtifact>,
  nativeLinkInputs: ReadonlyArray<NativeLinkInput.NativeLinkInput>,
  outputPath: string,
): Effect.fn.Return<ToolchainPlan.PlannedCommand, ToolchainError> {
  const plan = ToolchainPlan.nativeCommand(
    toolchain,
    artifactKind,
    target,
    generatedObjects.map((object) => object.path),
    nativeLinkInputs,
    outputPath,
  )
  if (plan._tag === 'UnsupportedNativePlan') return yield* unsupportedPlanError(plan)
  for (const object of generatedObjects) {
    if (object.target.id !== target.id) {
      return yield* linkError(
        plan,
        null,
        `linker input ${object.path} targets ${object.target.id}; expected ${target.id}`,
      )
    }
    const exists = yield* Effect.try({
      try: () => existsSync(object.path),
      catch: (cause) => linkError(plan, null, `cannot inspect linker input ${object.path}`, cause),
    })
    if (!exists) return yield* linkError(plan, null, `missing linker input: ${object.path}`)
  }
  for (const input of nativeLinkInputs) {
    const path = NativeLinkInput.path(input)
    if (path === undefined) continue
    const exists = yield* Effect.try({
      try: () => existsSync(path),
      catch: (cause) => linkError(plan, null, `cannot inspect linker input ${path}`, cause),
    })
    if (!exists) return yield* linkError(plan, null, `missing linker input: ${path}`)
  }
  return plan
})

export const NativeFinalizer = Object.freeze({
  finalize: Effect.fn('NativeToolchain.NativeFinalizer.finalize')(function* (
    toolchain: Toolchain,
    scope: BuildScope,
    artifactKind: ToolchainPlan.NativeArtifactKind,
    target: Target.Target,
    generatedObjects: ReadonlyArray<PathArtifact>,
    nativeLinkInputs: ReadonlyArray<NativeLinkInput.NativeLinkInput>,
    destination: string,
  ): Effect.fn.Return<FinalArtifact, ToolchainError> {
    let outputName = 'archive.a'
    if (artifactKind === 'NativeExecutable') outputName = 'linked-program'
    else if (artifactKind === 'NativeSharedLibrary') outputName = basename(destination)
    const outputPath = join(scope.root, outputName)
    const planned = yield* requireLinkInputs(
      toolchain,
      artifactKind,
      target,
      generatedObjects,
      nativeLinkInputs,
      outputPath,
    )
    const result = yield* Effect.try({
      try: () => spawnSync(planned.command, [...planned.arguments], { encoding: 'utf8' }),
      catch: (cause) => linkError(planned, null, '', cause),
    })
    const output = `${result.stdout ?? ''}${result.stderr ?? ''}${result.error?.message ?? ''}`
    if (result.error !== undefined || result.status !== 0) {
      return yield* linkError(planned, result.status, output, result.error)
    }
    yield* requirePath('NativeToolchain.NativeFinalizer.finalize', 'link', outputPath)
    const bytes = yield* Effect.try({
      try: () => readFileSync(outputPath),
      catch: (cause) =>
        storageError('NativeToolchain.NativeFinalizer.finalize', 'link', outputPath, cause),
    })
    const path = yield* atomicCommit(destination, bytes, {
      ...(artifactKind === 'NativeExecutable' ? { mode: 0o755 } : {}),
      stage: 'artifact-commit',
    })
    return Object.freeze({
      _tag: 'FinalArtifact',
      kind: artifactKind,
      path,
      bytes,
      target,
      planned,
    })
  }),
})

const hasWasmHeader = (bytes: Uint8Array): boolean =>
  bytes.length >= 8 &&
  bytes[0] === 0 &&
  bytes[1] === 97 &&
  bytes[2] === 115 &&
  bytes[3] === 109 &&
  bytes[4] === 1 &&
  bytes[5] === 0 &&
  bytes[6] === 0 &&
  bytes[7] === 0

const hasBytesAt = (bytes: Uint8Array, offset: number, expected: ReadonlyArray<number>): boolean =>
  expected.every((byte, index) => bytes[offset + index] === byte)

const uint16LittleEndian = (bytes: Uint8Array, offset: number): number | undefined => {
  const low = bytes[offset]
  const high = bytes[offset + 1]
  return low === undefined || high === undefined ? undefined : low | (high << 8)
}

const uint32LittleEndian = (bytes: Uint8Array, offset: number): number | undefined => {
  const low = uint16LittleEndian(bytes, offset)
  const high = uint16LittleEndian(bytes, offset + 2)
  return low === undefined || high === undefined ? undefined : low + high * 0x1_0000
}

const uint64LittleEndian = (bytes: Uint8Array, offset: number): number | undefined => {
  const low = uint32LittleEndian(bytes, offset)
  const high = uint32LittleEndian(bytes, offset + 4)
  if (low === undefined || high === undefined) return undefined
  const value = low + high * 0x1_0000_0000
  return Number.isSafeInteger(value) ? value : undefined
}

const isNativeObjectForTarget = (
  bytes: Uint8Array,
  offset: number,
  target: Target.Target,
): boolean => {
  if (target.id === 'aarch64-apple-darwin') {
    return (
      hasBytesAt(bytes, offset, [0xcf, 0xfa, 0xed, 0xfe]) &&
      uint32LittleEndian(bytes, offset + 4) === 0x0100_000c
    )
  }
  let elfMachine: number | undefined
  if (target.id === 'x86_64-unknown-linux-gnu') elfMachine = 62
  else if (target.id === 'aarch64-unknown-linux-gnu') elfMachine = 183
  return (
    elfMachine !== undefined &&
    hasBytesAt(bytes, offset, [0x7f, 0x45, 0x4c, 0x46]) &&
    bytes[offset + 4] === 2 &&
    bytes[offset + 5] === 1 &&
    uint16LittleEndian(bytes, offset + 18) === elfMachine
  )
}

const isNativeImageForKind = (
  bytes: Uint8Array,
  kind: 'NativeExecutable' | 'NativeSharedLibrary',
  target: Target.Target,
): boolean => {
  if (!isNativeObjectForTarget(bytes, 0, target)) return false
  if (target.id === 'aarch64-apple-darwin') {
    if (bytes.length < 32) return false
    const fileType = uint32LittleEndian(bytes, 12)
    return kind === 'NativeExecutable' ? fileType === 2 : fileType === 6
  }
  if (bytes.length < 64) return false
  const fileType = uint16LittleEndian(bytes, 16)
  if (fileType === 2) return kind === 'NativeExecutable'
  if (fileType !== 3) return false
  const programOffset = uint64LittleEndian(bytes, 32)
  const programEntrySize = uint16LittleEndian(bytes, 54)
  const programCount = uint16LittleEndian(bytes, 56)
  if (programOffset === undefined || programEntrySize === undefined || programCount === undefined)
    return false
  if (programEntrySize < 4 || programOffset + programEntrySize * programCount > bytes.length)
    return false
  let hasInterpreter = false
  for (let index = 0; index < programCount; index += 1) {
    if (uint32LittleEndian(bytes, programOffset + programEntrySize * index) === 3)
      hasInterpreter = true
  }
  return kind === 'NativeExecutable' ? hasInterpreter : !hasInterpreter
}

const asciiDecoder = new TextDecoder('ascii')

const asciiField = (bytes: Uint8Array, offset: number, length: number): string =>
  asciiDecoder.decode(bytes.subarray(offset, offset + length)).trim()

const isStaticArchiveForTarget = (bytes: Uint8Array, target: Target.Target): boolean => {
  if (!hasBytesAt(bytes, 0, [0x21, 0x3c, 0x61, 0x72, 0x63, 0x68, 0x3e, 0x0a])) return false
  let offset = 8
  let hasTargetObject = false
  while (offset + 60 <= bytes.length) {
    if (!hasBytesAt(bytes, offset + 58, [0x60, 0x0a])) return false
    const sizeText = asciiField(bytes, offset + 48, 10)
    if (!/^\d+$/.test(sizeText)) return false
    const size = Number(sizeText)
    const dataOffset = offset + 60
    if (!Number.isSafeInteger(size) || dataOffset + size > bytes.length) return false
    if (size % 2 === 1 && bytes[dataOffset + size] !== 0x0a) return false
    const name = asciiField(bytes, offset, 16)
    const extendedName = /^#1\/(\d+)$/.exec(name)
    const nameLength = extendedName === null ? 0 : Number(extendedName[1])
    if (!Number.isSafeInteger(nameLength) || nameLength > size) return false
    const objectOffset = dataOffset + nameLength
    let memberName = name
    if (extendedName !== null)
      memberName = asciiField(bytes, dataOffset, nameLength).replace(/\0+$/, '')
    const isIndex =
      memberName === '/' ||
      memberName === '//' ||
      memberName === '/SYM64/' ||
      memberName.startsWith('__.SYMDEF')
    if (!isIndex) {
      if (!isNativeObjectForTarget(bytes, objectOffset, target)) return false
      hasTargetObject = true
    }
    offset = dataOffset + size + (size % 2)
  }
  return offset === bytes.length && hasTargetObject
}

/** Whether finalized bytes match the requested artifact container, kind, and target. */
export const isCachedArtifact = (
  bytes: Uint8Array,
  kind: FinalArtifact['kind'],
  target: Target.Target,
): boolean => {
  if (kind === 'WebAssemblyModule')
    return target.id === 'wasm32-unknown-unknown' && hasWasmHeader(bytes)
  if (kind === 'NativeStaticLibrary') return isStaticArchiveForTarget(bytes, target)
  return isNativeImageForKind(bytes, kind, target)
}

export const finalizeWasm = Effect.fn('NativeToolchain.finalizeWasm')(function* (
  toolchain: Toolchain,
  scope: BuildScope,
  artifact: Backend.LlvmBitcodeArtifact,
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
  destination: string,
): Effect.fn.Return<FinalArtifact, ToolchainError> {
  const bitcode = yield* writeArtifact(scope, target, 'program.bc', artifact.bitcode)
  const outputPath = join(scope.root, 'program.wasm')
  const planned = ToolchainPlan.wasmCommand(
    toolchain.clang,
    target,
    profile,
    bitcode.path,
    outputPath,
  )
  if (artifact.target.id !== target.id) {
    return yield* processError(
      'NativeToolchain.finalizeWasm',
      'wasm-finalize',
      planned,
      null,
      `bitcode target ${artifact.target.id} does not match requested target ${target.id}`,
    )
  }
  yield* runPlanned('NativeToolchain.finalizeWasm', 'wasm-finalize', planned)
  yield* requirePath('NativeToolchain.finalizeWasm', 'wasm-finalize', outputPath)
  const bytes = yield* Effect.try({
    try: () => readFileSync(outputPath),
    catch: (cause) =>
      storageError('NativeToolchain.finalizeWasm', 'wasm-finalize', outputPath, cause),
  })
  if (!hasWasmHeader(bytes)) {
    return yield* processError(
      'NativeToolchain.finalizeWasm',
      'wasm-finalize',
      planned,
      0,
      'invalid WebAssembly output',
    )
  }
  const path = yield* atomicCommit(destination, bytes)
  return Object.freeze({
    _tag: 'FinalArtifact',
    kind: 'WebAssemblyModule',
    path,
    bytes,
    target,
    planned,
  })
})

export const commitWasm = Effect.fn('NativeToolchain.commitWasm')(function* (
  artifact: Backend.WebAssemblyModuleArtifact,
  destination: string,
): Effect.fn.Return<FinalArtifact, ToolchainError> {
  if (!hasWasmHeader(artifact.bytes)) {
    return yield* storageError(
      'NativeToolchain.commitWasm',
      'artifact-commit',
      destination,
      new TypeError('backend bytes are not a WebAssembly module'),
    )
  }
  const bytes = Uint8Array.from(artifact.bytes)
  const path = yield* atomicCommit(destination, bytes)
  return Object.freeze({
    _tag: 'FinalArtifact',
    kind: 'WebAssemblyModule',
    path,
    bytes,
    target: artifact.target,
  })
})

export const commitCachedArtifact = Effect.fn('NativeToolchain.commitCachedArtifact')(function* (
  bytes: Uint8Array,
  kind: FinalArtifact['kind'],
  target: Target.Target,
  destination: string,
): Effect.fn.Return<FinalArtifact, ToolchainError> {
  if (!isCachedArtifact(bytes, kind, target)) {
    return yield* storageError(
      'NativeToolchain.commitCachedArtifact',
      'artifact-commit',
      destination,
      new TypeError(`cached bytes are not a ${kind} artifact for ${target.id}`),
    )
  }
  const copy = Uint8Array.from(bytes)
  const path = yield* atomicCommit(destination, copy, {
    ...(kind === 'NativeExecutable' ? { mode: 0o755 } : {}),
    stage: 'artifact-commit',
  })
  return Object.freeze({ _tag: 'FinalArtifact', kind, path, bytes: copy, target })
})
