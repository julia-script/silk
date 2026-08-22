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
import { dirname, join, resolve } from 'node:path'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as Result from 'effect/Result'
import type * as Backend from './Backend.js'
import * as Target from './Target.js'
import * as ToolchainPlan from './ToolchainPlan.js'

export interface Toolchain {
  readonly _tag: 'Toolchain'
  readonly clang: string
  readonly shimCache?: ShimCache
  readonly artifactCache?: ArtifactCache
}

export interface ShimCache {
  readonly _tag: 'ShimCache'
  readonly get: (key: string) => Effect.Effect<Uint8Array | undefined, ToolchainError>
  readonly set: (key: string, bytes: Uint8Array) => Effect.Effect<void, ToolchainError>
  readonly stats: () => ShimCacheStats
}

export interface ShimCacheStats {
  readonly entries: number
  readonly hits: number
  readonly misses: number
}

export const makeShimCache = (): ShimCache => {
  const objects = new Map<string, Uint8Array>()
  let hits = 0
  let misses = 0
  return Object.freeze({
    _tag: 'ShimCache',
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

export const shimCacheStats = (self: ShimCache): ShimCacheStats => self.stats()

export type Stage =
  | 'host-target'
  | 'cache-key'
  | 'cache-read'
  | 'cache-write'
  | 'scope-acquire'
  | 'scope-cleanup'
  | 'scope-write'
  | 'object'
  | 'shim'
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
  stage: 'object' | 'shim' | 'wasm-finalize',
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
    operation: 'NativeToolchain.ClangLinker.link',
    stage: 'link',
    message: `NativeToolchain.ClangLinker.link failed: ${output}`,
    reason: {
      _tag: 'LinkFailed',
      planned,
      status,
      output,
      ...(cause === undefined ? {} : { cause }),
    },
  })

export interface ArtifactCache {
  readonly _tag: 'ArtifactCache'
  readonly get: (key: string) => Effect.Effect<Uint8Array | undefined, ToolchainError>
  readonly set: (key: string, bytes: Uint8Array) => Effect.Effect<void, ToolchainError>
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
  return yield* operation.pipe(
    Effect.mapError((cause) =>
      cause instanceof ToolchainError
        ? cause
        : storageError('NativeToolchain.ArtifactCache.get', 'cache-read', key, cause),
    ),
  )
})

/** Writes a caller-supplied artifact cache without allowing callback throws past the boundary. */
export const writeArtifactCache = Effect.fnUntraced(function* (
  cache: ArtifactCache,
  key: string,
  bytes: Uint8Array,
): Effect.fn.Return<void, ToolchainError> {
  const operation = yield* Effect.try({
    try: () => cache.set(key, bytes),
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
      catch: (cause) => cause,
    }),
  )
  if (Result.isSuccess(first)) return
  const retry = yield* Effect.result(
    Effect.try({
      try: () => cleanup.remove(path, options),
      catch: (cause) => cause,
    }),
  )
  if (Result.isSuccess(retry)) return
  const fallback = yield* Effect.result(
    Effect.try({
      try: () => nodeCleanup.remove(path, options),
      catch: (cause) => cause,
    }),
  )
  return yield* storageError('NativeToolchain.cleanupPath', stage, path, {
    first: first.failure,
    retry: retry.failure,
    ...(Result.isFailure(fallback) ? { fallback: fallback.failure } : {}),
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

export const defaultArtifactCache = (): ArtifactCache => {
  const directory = process.env.SILK_NATIVE_CACHE_DIR
  if (directory !== undefined && directory !== '') return makeDiskArtifactCache(directory)
  return Object.freeze({
    _tag: 'ArtifactCache',
    get: (key: string) => Effect.succeed(processArtifactCache.get(key)),
    set: (key: string, bytes: Uint8Array) =>
      Effect.sync(() => {
        processArtifactCache.set(key, Uint8Array.from(bytes))
      }),
  })
}

const clangVersions = new Map<string, string>()

const clangVersionOf = Effect.fnUntraced(function* (
  clang: string,
): Effect.fn.Return<string, ToolchainError> {
  const cached = clangVersions.get(clang)
  if (cached !== undefined) return cached
  const planned: ToolchainPlan.PlannedCommand = Object.freeze({
    _tag: 'PlannedCommand',
    stage: 'Object',
    command: clang,
    arguments: Object.freeze(['--version']),
    target: Target.wasm32UnknownUnknown,
  })
  const result = yield* Effect.try({
    try: () => spawnSync(clang, ['--version'], { encoding: 'utf8' }),
    catch: (cause) =>
      processError('NativeToolchain.clangVersionOf', 'object', planned, null, '', cause),
  })
  if (result.error !== undefined || result.status !== 0) {
    return yield* processError(
      'NativeToolchain.clangVersionOf',
      'object',
      planned,
      result.status,
      `${result.stdout ?? ''}${result.stderr ?? ''}${result.error?.message ?? ''}`,
      result.error,
    )
  }
  const version = result.stdout.split('\n', 1)[0] ?? ''
  clangVersions.set(clang, version)
  return version
})

export const artifactCacheKey = Effect.fn('NativeToolchain.artifactCacheKey')(function* (
  toolchain: Toolchain,
  kind: FinalArtifact['kind'],
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
  bitcode: Uint8Array | string,
  shimSource: string,
): Effect.fn.Return<string, ToolchainError> {
  const version = yield* clangVersionOf(toolchain.clang)
  return yield* Effect.try({
    try: () => {
      const digest = createHash('sha256')
      for (const value of [kind, target.id, profile, toolchain.clang, version, shimSource]) {
        digest.update(value)
        digest.update('\0')
      }
      digest.update(bitcode)
      return `${digest.digest('hex')}.${kind === 'NativeExecutable' ? 'bin' : 'wasm'}`
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

export interface Executable {
  readonly _tag: 'Executable'
  readonly path: string
  readonly bytes: Uint8Array
  readonly target: Target.Target
  readonly planned: ToolchainPlan.PlannedCommand
}

export interface FinalArtifact {
  readonly _tag: 'FinalArtifact'
  readonly kind: 'NativeExecutable' | 'WebAssemblyModule'
  readonly path: string
  readonly bytes: Uint8Array
  readonly target: Target.Target
  readonly planned?: ToolchainPlan.PlannedCommand
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
        ? Effect.succeed(undefined)
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

const readShimCache = Effect.fnUntraced(function* (
  cache: ShimCache,
  key: string,
): Effect.fn.Return<Uint8Array | undefined, ToolchainError> {
  const operation = yield* Effect.try({
    try: () => cache.get(key),
    catch: (cause) => storageError('NativeToolchain.ShimCache.get', 'cache-read', key, cause),
  })
  return yield* operation.pipe(
    Effect.mapError((cause) =>
      storageError('NativeToolchain.ShimCache.get', 'cache-read', key, cause),
    ),
  )
})

const writeShimCache = Effect.fnUntraced(function* (
  cache: ShimCache,
  key: string,
  bytes: Uint8Array,
): Effect.fn.Return<void, ToolchainError> {
  const operation = yield* Effect.try({
    try: () => cache.set(key, bytes),
    catch: (cause) => storageError('NativeToolchain.ShimCache.set', 'cache-write', key, cause),
  })
  return yield* operation.pipe(
    Effect.mapError((cause) =>
      storageError('NativeToolchain.ShimCache.set', 'cache-write', key, cause),
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
  stage: 'object' | 'shim' | 'wasm-finalize',
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

export const compileShim = Effect.fn('NativeToolchain.compileShim')(function* (
  toolchain: Toolchain,
  scope: BuildScope,
  target: Target.Target,
  termination: Backend.Termination,
  nativeRuntimeSymbols: ReadonlyArray<string> = Object.freeze([]),
): Effect.fn.Return<ObjectArtifact, ToolchainError> {
  const sourceText = ToolchainPlan.shimSource(termination, nativeRuntimeSymbols)
  const cacheKey = `${toolchain.clang}\u0000${target.id}\u0000${sourceText}`
  const objectPath = join(scope.root, 'silk_shim.o')
  const source = yield* writeArtifact(scope, target, 'silk_shim.c', sourceText)
  const planned = ToolchainPlan.shimCommand(toolchain.clang, target, source.path, objectPath)
  const cached =
    toolchain.shimCache === undefined
      ? undefined
      : yield* readShimCache(toolchain.shimCache, cacheKey)
  if (cached !== undefined) {
    yield* writeArtifact(scope, target, 'silk_shim.o', cached)
  } else {
    yield* runPlanned('NativeToolchain.compileShim', 'shim', planned)
    yield* requirePath('NativeToolchain.compileShim', 'shim', objectPath)
    const bytes = yield* Effect.try({
      try: () => readFileSync(objectPath),
      catch: (cause) => storageError('NativeToolchain.compileShim', 'shim', objectPath, cause),
    })
    if (toolchain.shimCache !== undefined)
      yield* writeShimCache(toolchain.shimCache, cacheKey, bytes)
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

export const ClangLinker = Object.freeze({
  link: Effect.fn('NativeToolchain.ClangLinker.link')(function* (
    toolchain: Toolchain,
    scope: BuildScope,
    target: Target.Target,
    objects: ReadonlyArray<PathArtifact>,
    libraries: ReadonlyArray<string>,
    destination: string,
  ): Effect.fn.Return<Executable, ToolchainError> {
    const outputPath = join(scope.root, 'linked-program')
    const planned = ToolchainPlan.linkCommand(
      toolchain.clang,
      target,
      objects.map((object) => object.path),
      libraries,
      outputPath,
    )
    for (const object of objects) {
      if (object.target.id !== target.id) {
        return yield* linkError(
          planned,
          null,
          `linker input ${object.path} targets ${object.target.id}; expected ${target.id}`,
        )
      }
      const exists = yield* Effect.try({
        try: () => existsSync(object.path),
        catch: (cause) =>
          linkError(planned, null, `cannot inspect linker input ${object.path}`, cause),
      })
      if (!exists) return yield* linkError(planned, null, `missing linker input: ${object.path}`)
    }
    const result = yield* Effect.try({
      try: () => spawnSync(planned.command, [...planned.arguments], { encoding: 'utf8' }),
      catch: (cause) => linkError(planned, null, '', cause),
    })
    const output = `${result.stdout ?? ''}${result.stderr ?? ''}${result.error?.message ?? ''}`
    if (result.error !== undefined || result.status !== 0) {
      return yield* linkError(planned, result.status, output, result.error)
    }
    yield* requirePath('NativeToolchain.ClangLinker.link', 'link', outputPath)
    const bytes = yield* Effect.try({
      try: () => readFileSync(outputPath),
      catch: (cause) => storageError('NativeToolchain.ClangLinker.link', 'link', outputPath, cause),
    })
    const path = yield* atomicCommit(destination, bytes, {
      mode: 0o755,
      stage: 'artifact-commit',
    })
    return Object.freeze({ _tag: 'Executable', path, bytes, target, planned })
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
  const copy = Uint8Array.from(bytes)
  const path = yield* atomicCommit(destination, copy, {
    ...(kind === 'NativeExecutable' ? { mode: 0o755 } : {}),
    stage: 'artifact-commit',
  })
  return Object.freeze({ _tag: 'FinalArtifact', kind, path, bytes: copy, target })
})
