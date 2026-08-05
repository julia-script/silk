import { spawnSync } from 'node:child_process'
import { copyFileSync, existsSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { arch, platform, tmpdir } from 'node:os'
import { join, resolve } from 'node:path'
import type * as Mir from './Mir.js'
import * as ToolchainPlan from './ToolchainPlan.js'

/**
 * Pinned-Clang orchestration: build scopes owning path-backed intermediates, object emission
 * completing the backend contract, the runtime shim, and the `NativeLinker` service with its
 * `ClangLinker` implementation. Node-only by construction — reachable as a deep import so the
 * package root stays browser-safe. Failures are data with full command provenance.
 */

/** Derives the target layout for the machine running the driver. */
export const hostLayout = (): Mir.TargetLayout => {
  const cpu = arch() === 'arm64' ? 'arm64' : 'x86_64'
  const triple =
    platform() === 'darwin'
      ? `${cpu === 'arm64' ? 'arm64' : 'x86_64'}-apple-darwin`
      : `${cpu === 'arm64' ? 'aarch64' : 'x86_64'}-unknown-linux-gnu`
  return Object.freeze({
    _tag: 'TargetLayout',
    triple,
    pointerWidth: 64,
    endianness: 'little',
    i32: Object.freeze({ size: 4, alignment: 4 }),
  })
}

/** The caller-pinned external toolchain. No PATH discovery is performed. */
export interface Toolchain {
  readonly _tag: 'Toolchain'
  readonly clang: string
}

/** One owned, path-backed artifact tied to a build scope. */
export interface PathArtifact {
  readonly _tag: 'PathArtifact'
  readonly scope: string
  readonly path: string
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
  readonly planned: ToolchainPlan.PlannedCommand
}

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
  fileName: string,
  bytes: Uint8Array | string,
): PathArtifact => {
  const path = join(scope.root, fileName)
  writeFileSync(path, bytes)
  return Object.freeze({ _tag: 'PathArtifact', scope: scope.name, path })
}

/** Promotes a scope-owned artifact to a durable destination that survives scope exit. */
export const promote = (artifact: PathArtifact, destination: string): string => {
  const target = resolve(destination)
  copyFileSync(artifact.path, target)
  return target
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
): ToolchainFailure => Object.freeze({ _tag: 'ToolchainFailure', planned, status, output })

/**
 * Completes the backend's object contract: writes the bitcode into the scope and invokes the
 * pinned Clang with `-c` under the fixed profile, producing one relocatable target object.
 */
export const emitObject = (
  toolchain: Toolchain,
  scope: BuildScope,
  bitcode: Uint8Array,
  profile: ToolchainPlan.OptimizationProfile,
  baseName = 'program',
): ObjectArtifact | ToolchainFailure => {
  const bitcodeArtifact = writeArtifact(scope, `${baseName}.bc`, bitcode)
  const objectPath = join(scope.root, `${baseName}.o`)
  const planned = ToolchainPlan.objectCommand(
    toolchain.clang,
    profile,
    bitcodeArtifact.path,
    objectPath,
  )
  const result = runPlanned(planned)
  if (result.status !== 0 || !existsSync(objectPath)) {
    return failure(planned, result.status, result.output)
  }
  return Object.freeze({
    _tag: 'ObjectArtifact',
    artifact: Object.freeze({ _tag: 'PathArtifact', scope: scope.name, path: objectPath }),
    planned,
  })
}

/** Compiles the minimal runtime shim inside the scope with the pinned Clang. */
export const compileShim = (
  toolchain: Toolchain,
  scope: BuildScope,
): ObjectArtifact | ToolchainFailure => {
  const source = writeArtifact(scope, 'silk_shim.c', ToolchainPlan.shimSource)
  const objectPath = join(scope.root, 'silk_shim.o')
  const planned = ToolchainPlan.shimCommand(toolchain.clang, source.path, objectPath)
  const result = runPlanned(planned)
  if (result.status !== 0 || !existsSync(objectPath)) {
    return failure(planned, result.status, result.output)
  }
  return Object.freeze({
    _tag: 'ObjectArtifact',
    artifact: Object.freeze({ _tag: 'PathArtifact', scope: scope.name, path: objectPath }),
    planned,
  })
}

/** The nominal native-linker service contract. */
export interface NativeLinker {
  readonly link: (
    toolchain: Toolchain,
    objects: ReadonlyArray<PathArtifact>,
    libraries: ReadonlyArray<string>,
    destination: string,
  ) => Executable | ToolchainFailure
}

/** The bootstrap linker driving the pinned Clang link driver with structured arguments. */
export const ClangLinker: NativeLinker = Object.freeze({
  link: (
    toolchain: Toolchain,
    objects: ReadonlyArray<PathArtifact>,
    libraries: ReadonlyArray<string>,
    destination: string,
  ): Executable | ToolchainFailure => {
    const target = resolve(destination)
    const planned = ToolchainPlan.linkCommand(
      toolchain.clang,
      objects.map((object) => object.path),
      libraries,
      target,
    )
    for (const object of objects) {
      if (!existsSync(object.path)) {
        return failure(planned, null, `missing linker input: ${object.path}`)
      }
    }
    const result = runPlanned(planned)
    if (result.status !== 0 || !existsSync(target)) {
      return failure(planned, result.status, result.output)
    }
    return Object.freeze({ _tag: 'Executable', path: target, planned })
  },
})
