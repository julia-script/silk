import { spawnSync } from 'node:child_process'
import { createHash } from 'node:crypto'
import {
  copyFileSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  renameSync,
  rmSync,
  writeFileSync,
} from 'node:fs'
import { tmpdir } from 'node:os'
import { join, resolve } from 'node:path'
import type * as Backend from './Backend.js'
import type * as Target from './Target.js'
import * as ToolchainPlan from './ToolchainPlan.js'

/**
 * Pinned-Clang and durable-artifact orchestration: build scopes own path-backed intermediates,
 * native object/shim/link work and LLVM-Wasm finalization share one process boundary, while final
 * direct-Wasm bytes use the same atomic storage boundary without invoking Clang. The
 * `NativeLinker` service with its
 * `ClangLinker` implementation. Node-only by construction — reachable as a deep import so the
 * package root stays browser-safe. Failures are data with full command provenance.
 */

/** The caller-pinned external toolchain. No PATH discovery is performed. */
export interface Toolchain {
  readonly _tag: 'Toolchain'
  readonly clang: string
  readonly shimCache?: ShimCache
  readonly artifactCache?: ArtifactCache
}

/** Process-local compiled shim bytes shared explicitly across independent build scopes. */
export interface ShimCache {
  readonly _tag: 'ShimCache'
  readonly get: (key: string) => Uint8Array | undefined
  readonly set: (key: string, bytes: Uint8Array) => void
  readonly stats: () => ShimCacheStats
}

/** Operational counters for observing whether a shared shim cache is effective. */
export interface ShimCacheStats {
  readonly entries: number
  readonly hits: number
  readonly misses: number
}

/** Makes an explicitly owned, process-local cache of path-independent shim object bytes. */
export const makeShimCache = (): ShimCache => {
  const objects = new Map<string, Uint8Array>()
  let hits = 0
  let misses = 0
  return Object.freeze({
    _tag: 'ShimCache',
    get: (key: string) => {
      const bytes = objects.get(key)
      if (bytes === undefined) misses += 1
      else hits += 1
      return bytes
    },
    set: (key: string, bytes: Uint8Array) => {
      objects.set(key, Uint8Array.from(bytes))
    },
    stats: () => Object.freeze({ entries: objects.size, hits, misses }),
  })
}

/** Reads immutable counters from a shared shim cache. */
export const shimCacheStats = (self: ShimCache): ShimCacheStats => self.stats()

/**
 * Content-addressed storage of finished artifact bytes, keyed by everything that can change the
 * output. A hit skips the external toolchain entirely.
 */
export interface ArtifactCache {
  readonly _tag: 'ArtifactCache'
  readonly get: (key: string) => Uint8Array | undefined
  readonly set: (key: string, bytes: Uint8Array) => void
}

/** Makes a durable artifact cache under `directory`, shared across processes. */
export const makeDiskArtifactCache = (directory: string): ArtifactCache => {
  const root = resolve(directory)
  return Object.freeze({
    _tag: 'ArtifactCache',
    get: (key: string) => {
      const path = join(root, key)
      if (!existsSync(path)) return undefined
      try {
        return readFileSync(path)
      } catch {
        // ponytail: an unreadable entry is a miss, not a build failure.
        return undefined
      }
    },
    set: (key: string, bytes: Uint8Array) => {
      try {
        mkdirSync(root, { recursive: true })
        // Stage then rename, so a concurrent reader never observes a partial entry.
        const path = join(root, key)
        const temporary = `${path}.silk-tmp-${process.pid}`
        writeFileSync(temporary, bytes)
        renameSync(temporary, path)
      } catch {
        // ponytail: caching is an optimization; a write failure must not fail the build.
      }
    },
  })
}

const processArtifactCache = new Map<string, Uint8Array>()

/**
 * The artifact cache used when a toolchain pins none of its own: the durable disk cache under
 * `SILK_NATIVE_CACHE_DIR` when that is set, otherwise process-local memory. The variable is how
 * test runs and CI share compiled artifacts across processes without threading a cache through
 * every call site.
 */
export const defaultArtifactCache = (): ArtifactCache => {
  const directory = process.env.SILK_NATIVE_CACHE_DIR
  if (directory !== undefined && directory !== '') return makeDiskArtifactCache(directory)
  return Object.freeze({
    _tag: 'ArtifactCache',
    get: (key: string) => processArtifactCache.get(key),
    set: (key: string, bytes: Uint8Array) => {
      processArtifactCache.set(key, Uint8Array.from(bytes))
    },
  })
}

const clangVersions = new Map<string, string>()

/**
 * The first line of `clang --version`, memoized per path. The cache key needs the version, not
 * just the path: a durable cache outlives a toolchain upgrade installed at the same location.
 * A failed probe contributes the empty string — the path still participates in the key.
 */
const clangVersionOf = (clang: string): string => {
  const cached = clangVersions.get(clang)
  if (cached !== undefined) return cached
  let version = ''
  try {
    const probe = spawnSync(clang, ['--version'], { encoding: 'utf8' })
    version = probe.status === 0 ? (probe.stdout.split('\n', 1)[0] ?? '') : ''
  } catch {
    version = ''
  }
  clangVersions.set(clang, version)
  return version
}

/**
 * Derives the cache identity of a finished artifact. Every input that can change the emitted
 * bytes participates, so a hit is only ever served for a byte-identical build.
 */
export const artifactCacheKey = (
  toolchain: Toolchain,
  kind: FinalArtifact['kind'],
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
  bitcode: Uint8Array | string,
  shimSource: string,
): string => {
  const digest = createHash('sha256')
  digest.update(kind)
  digest.update('\0')
  digest.update(target.triple)
  digest.update('\0')
  digest.update(profile)
  digest.update('\0')
  digest.update(toolchain.clang)
  digest.update('\0')
  digest.update(clangVersionOf(toolchain.clang))
  digest.update('\0')
  digest.update(shimSource)
  digest.update('\0')
  digest.update(bitcode)
  return `${digest.digest('hex')}.${kind === 'NativeExecutable' ? 'bin' : 'wasm'}`
}

/** Commits cached artifact bytes straight to their destination, bypassing the toolchain. */
export const commitCachedArtifact = (
  bytes: Uint8Array,
  kind: FinalArtifact['kind'],
  target: Target.Target,
  destination: string,
): FinalArtifact | StorageFailure => {
  const path = resolve(destination)
  const temporary = `${path}.silk-tmp-${process.pid}`
  try {
    // A cached native executable must be restored runnable; bytes alone do not carry the mode.
    writeFileSync(temporary, bytes, kind === 'NativeExecutable' ? { mode: 0o755 } : undefined)
    renameSync(temporary, path)
    return Object.freeze({ _tag: 'FinalArtifact', kind, path, target })
  } catch (cause) {
    rmSync(temporary, { force: true })
    return storageFailure(path, cause)
  }
}

/** One owned, path-backed artifact tied to a build scope. */
export interface PathArtifact {
  readonly _tag: 'PathArtifact'
  readonly scope: string
  readonly path: string
  readonly target: Target.Target
}

/** One named build scope owning its intermediates until exit. */
export interface BuildScope {
  readonly _tag: 'BuildScope'
  readonly name: string
  readonly root: string
}

/** A process failure retained as data: exact command, arguments, status, and output. */
export interface ToolchainFailure {
  readonly _tag: 'ToolchainFailure'
  readonly planned: ToolchainPlan.PlannedCommand
  readonly reason:
    | { readonly _tag: 'ProcessFailure' }
    | { readonly _tag: 'MissingInput'; readonly path: string }
    | {
        readonly _tag: 'TargetMismatch'
        readonly expected: Target.Id
        readonly actual: Target.Id
      }
  readonly status: number | null
  readonly output: string
}

/** A successful object emission with its provenance. */
export interface ObjectArtifact {
  readonly _tag: 'ObjectArtifact'
  readonly artifact: PathArtifact
  readonly planned: ToolchainPlan.PlannedCommand
}

/** A successfully linked executable with its provenance. */
export interface Executable {
  readonly _tag: 'Executable'
  readonly path: string
  readonly target: Target.Target
  readonly planned: ToolchainPlan.PlannedCommand
}

/** A durable artifact produced after backend emission. */
export interface FinalArtifact {
  readonly _tag: 'FinalArtifact'
  readonly kind: 'NativeExecutable' | 'WebAssemblyModule'
  readonly path: string
  readonly target: Target.Target
  readonly planned?: ToolchainPlan.PlannedCommand
}

/** Durable artifact storage failed outside an external process. */
export interface StorageFailure {
  readonly _tag: 'StorageFailure'
  readonly operation: 'NativeToolchain.commit'
  readonly destination: string
  readonly message: string
  readonly cause: unknown
}

export type FinalizationFailure = ToolchainFailure | StorageFailure

/**
 * Runs one function inside a named build scope. The scope's directory and every unpromoted
 * artifact are removed at exit — after success and failure alike — unless `saveTemps` retains
 * them for inspection.
 */
export const withBuildScope = <A>(
  name: string,
  run: (scope: BuildScope) => A,
  options: { readonly saveTemps?: boolean } = {},
): A => {
  const root = mkdtempSync(join(tmpdir(), `silk-${name.replace(/[^A-Za-z0-9_-]/g, '_')}-`))
  const scope: BuildScope = Object.freeze({ _tag: 'BuildScope', name, root })
  try {
    return run(scope)
  } finally {
    if (options.saveTemps !== true) {
      rmSync(root, { recursive: true, force: true })
    }
  }
}

/** Writes bytes to a scope-owned path artifact. */
export const writeArtifact = (
  scope: BuildScope,
  target: Target.Target,
  fileName: string,
  bytes: Uint8Array | string,
): PathArtifact => {
  const path = join(scope.root, fileName)
  writeFileSync(path, bytes)
  return Object.freeze({ _tag: 'PathArtifact', scope: scope.name, path, target })
}

/** Promotes a scope-owned artifact to a durable destination that survives scope exit. */
export const promote = (artifact: PathArtifact, destination: string): string => {
  const target = resolve(destination)
  copyFileSync(artifact.path, target)
  return target
}

const storageFailure = (destination: string, cause: unknown): StorageFailure =>
  Object.freeze({
    _tag: 'StorageFailure',
    operation: 'NativeToolchain.commit',
    destination,
    message: `Cannot commit build artifact to ${destination}`,
    cause,
  })

/** Atomically commits a scope-owned file through a same-directory temporary sibling. */
const commit = (artifact: PathArtifact, destination: string): string | StorageFailure => {
  const target = resolve(destination)
  const temporary = `${target}.silk-tmp-${process.pid}`
  try {
    copyFileSync(artifact.path, temporary)
    renameSync(temporary, target)
    return target
  } catch (cause) {
    rmSync(temporary, { force: true })
    return storageFailure(target, cause)
  }
}

const runPlanned = (
  planned: ToolchainPlan.PlannedCommand,
): { readonly status: number | null; readonly output: string } => {
  const result = spawnSync(planned.command, [...planned.arguments], { encoding: 'utf8' })
  const output = `${result.stdout ?? ''}${result.stderr ?? ''}${result.error?.message ?? ''}`
  return { status: result.status, output }
}

const failure = (
  planned: ToolchainPlan.PlannedCommand,
  status: number | null,
  output: string,
  reason: ToolchainFailure['reason'] = { _tag: 'ProcessFailure' },
): ToolchainFailure => Object.freeze({ _tag: 'ToolchainFailure', planned, reason, status, output })

/**
 * Completes the backend's object contract: writes the bitcode into the scope and invokes the
 * pinned Clang with `-c` under the fixed profile, producing one relocatable target object.
 */
export const emitObject = (
  toolchain: Toolchain,
  scope: BuildScope,
  artifact: Backend.LlvmBitcodeArtifact,
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
  baseName = 'program',
): ObjectArtifact | ToolchainFailure => {
  const objectPath = join(scope.root, `${baseName}.o`)
  const planned = ToolchainPlan.objectCommand(
    toolchain.clang,
    target,
    profile,
    join(scope.root, `${baseName}.bc`),
    objectPath,
  )
  if (artifact.target.id !== target.id) {
    return failure(
      planned,
      null,
      `bitcode target ${artifact.target.id} does not match requested target ${target.id}`,
      { _tag: 'TargetMismatch', expected: target.id, actual: artifact.target.id },
    )
  }
  writeArtifact(scope, target, `${baseName}.bc`, artifact.bitcode)
  const result = runPlanned(planned)
  if (result.status !== 0 || !existsSync(objectPath)) {
    return failure(planned, result.status, result.output)
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
}

/** Compiles the minimal runtime shim inside the scope with the pinned Clang. */
export const compileShim = (
  toolchain: Toolchain,
  scope: BuildScope,
  target: Target.Target,
  termination: Backend.Termination,
  nativeRuntimeSymbols: ReadonlyArray<string> = Object.freeze([]),
): ObjectArtifact | ToolchainFailure => {
  const sourceText = ToolchainPlan.shimSource(termination, nativeRuntimeSymbols)
  const cacheKey = `${toolchain.clang}\u0000${target.id}\u0000${sourceText}`
  const objectPath = join(scope.root, 'silk_shim.o')
  const source = writeArtifact(scope, target, 'silk_shim.c', sourceText)
  const planned = ToolchainPlan.shimCommand(toolchain.clang, target, source.path, objectPath)
  const cached = toolchain.shimCache?.get(cacheKey)
  if (cached !== undefined) {
    writeArtifact(scope, target, 'silk_shim.o', cached)
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
  }
  const result = runPlanned(planned)
  if (result.status !== 0 || !existsSync(objectPath)) {
    return failure(planned, result.status, result.output)
  }
  toolchain.shimCache?.set(cacheKey, readFileSync(objectPath))
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
}

/** The nominal native-linker service contract. */
export interface NativeLinker {
  readonly link: (
    toolchain: Toolchain,
    target: Target.Target,
    objects: ReadonlyArray<PathArtifact>,
    libraries: ReadonlyArray<string>,
    destination: string,
  ) => Executable | ToolchainFailure
}

/** The bootstrap linker driving the pinned Clang link driver with structured arguments. */
export const ClangLinker: NativeLinker = Object.freeze({
  link: (
    toolchain: Toolchain,
    target: Target.Target,
    objects: ReadonlyArray<PathArtifact>,
    libraries: ReadonlyArray<string>,
    destination: string,
  ): Executable | ToolchainFailure => {
    const destinationPath = resolve(destination)
    const temporaryPath = `${destinationPath}.silk-tmp-${process.pid}`
    const planned = ToolchainPlan.linkCommand(
      toolchain.clang,
      target,
      objects.map((object) => object.path),
      libraries,
      temporaryPath,
    )
    for (const object of objects) {
      if (object.target.id !== target.id) {
        return failure(
          planned,
          null,
          `linker input ${object.path} targets ${object.target.id}; expected ${target.id}`,
          { _tag: 'TargetMismatch', expected: target.id, actual: object.target.id },
        )
      }
      if (!existsSync(object.path)) {
        return failure(planned, null, `missing linker input: ${object.path}`, {
          _tag: 'MissingInput',
          path: object.path,
        })
      }
    }
    const result = runPlanned(planned)
    if (result.status !== 0 || !existsSync(temporaryPath)) {
      rmSync(temporaryPath, { force: true })
      return failure(planned, result.status, result.output)
    }
    try {
      renameSync(temporaryPath, destinationPath)
    } catch (cause) {
      rmSync(temporaryPath, { force: true })
      return failure(planned, null, `cannot commit linked executable: ${String(cause)}`)
    }
    return Object.freeze({
      _tag: 'Executable',
      path: destinationPath,
      target: planned.target,
      planned,
    })
  },
})

const hasWasmHeader = (path: string): boolean => {
  const bytes = readFileSync(path)
  return (
    bytes.length >= 8 &&
    bytes[0] === 0 &&
    bytes[1] === 97 &&
    bytes[2] === 115 &&
    bytes[3] === 109 &&
    bytes[4] === 1 &&
    bytes[5] === 0 &&
    bytes[6] === 0 &&
    bytes[7] === 0
  )
}

/** Finalizes LLVM bitcode as a standalone WebAssembly module and atomically commits it. */
export const finalizeWasm = (
  toolchain: Toolchain,
  scope: BuildScope,
  artifact: Backend.LlvmBitcodeArtifact,
  target: Target.Target,
  profile: ToolchainPlan.OptimizationProfile,
  destination: string,
): FinalArtifact | FinalizationFailure => {
  try {
    const bitcode = writeArtifact(scope, target, 'program.bc', artifact.bitcode)
    const outputPath = join(scope.root, 'program.wasm')
    const planned = ToolchainPlan.wasmCommand(
      toolchain.clang,
      target,
      profile,
      bitcode.path,
      outputPath,
    )
    if (artifact.target.id !== target.id) {
      return failure(
        planned,
        null,
        `bitcode target ${artifact.target.id} does not match requested target ${target.id}`,
        {
          _tag: 'TargetMismatch',
          expected: target.id,
          actual: artifact.target.id,
        },
      )
    }
    const result = runPlanned(planned)
    if (result.status !== 0 || !existsSync(outputPath) || !hasWasmHeader(outputPath)) {
      return failure(planned, result.status, result.output)
    }
    const committed = commit(
      Object.freeze({ _tag: 'PathArtifact', scope: scope.name, path: outputPath, target }),
      destination,
    )
    if (typeof committed !== 'string') return committed
    return Object.freeze({
      _tag: 'FinalArtifact',
      kind: 'WebAssemblyModule',
      path: committed,
      target,
      planned,
    })
  } catch (cause) {
    return storageFailure(destination, cause)
  }
}

/** Atomically commits already-validated direct WebAssembly bytes without invoking Clang. */
export const commitWasm = (
  scope: BuildScope,
  artifact: Backend.WebAssemblyModuleArtifact,
  destination: string,
): FinalArtifact | StorageFailure => {
  try {
    const staged = writeArtifact(scope, artifact.target, 'program.wasm', artifact.bytes)
    if (!hasWasmHeader(staged.path)) {
      return storageFailure(
        destination,
        new TypeError('backend bytes are not a WebAssembly module'),
      )
    }
    const committed = commit(staged, destination)
    if (typeof committed !== 'string') return committed
    return Object.freeze({
      _tag: 'FinalArtifact',
      kind: 'WebAssemblyModule',
      path: committed,
      target: artifact.target,
    })
  } catch (cause) {
    return storageFailure(destination, cause)
  }
}
