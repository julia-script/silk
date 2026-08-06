import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import { parse, TomlDate, type TomlTable, type TomlValue } from 'smol-toml'
import * as SourceEntry from './SourceEntry.js'

/** The conventional project manifest name used by upward discovery. */
export const manifestName = 'silk.toml'

/** A loaded project with every manifest-relative path made absolute and its entry materialized. */
export interface Project {
  readonly _tag: 'Project'
  readonly name: string
  readonly manifestPath: string
  readonly directory: string
  readonly entry: SourceEntry.SourceEntry
}

export type ProjectErrorReason =
  | { readonly _tag: 'ManifestNotFound'; readonly startDirectory: string }
  | { readonly _tag: 'InvalidManifest'; readonly detail: string }
  | { readonly _tag: 'InvalidEntry'; readonly error: SourceEntry.SourceEntryError }
  | { readonly _tag: 'WrappedFailure'; readonly cause: unknown }

/** Project discovery, decoding, validation, or entry materialization failed. */
export class ProjectError extends Data.TaggedError('ProjectError')<{
  readonly operation: 'Project.discover' | 'Project.load'
  readonly manifestPath: string
  readonly message: string
  readonly reason: ProjectErrorReason
}> {}

export interface LoadOptions {
  readonly workingDirectory?: string
  readonly manifestPath?: string
}

const packageNamePattern = /^[a-z][a-z0-9-]*$/

/** Whether a name is portable across the initial artifact and package conventions. */
export const isPackageName = (name: string): boolean => packageNamePattern.test(name)

const isTable = (value: TomlValue | undefined): value is TomlTable =>
  typeof value === 'object' &&
  value !== null &&
  !Array.isArray(value) &&
  !(value instanceof TomlDate)

const invalidManifest = (manifestPath: string, detail: string): ProjectError =>
  new ProjectError({
    operation: 'Project.load',
    manifestPath,
    message: `Invalid Silk project manifest ${manifestPath}: ${detail}`,
    reason: { _tag: 'InvalidManifest', detail },
  })

const decodePackage = Effect.fnUntraced(function* (manifestPath: string, text: string) {
  const document = yield* Effect.try({
    try: () => parse(text),
    catch: (cause) =>
      new ProjectError({
        operation: 'Project.load',
        manifestPath,
        message: `Cannot parse Silk project manifest ${manifestPath}`,
        reason: { _tag: 'WrappedFailure', cause },
      }),
  })
  const packageTable = document.package
  if (!isTable(packageTable)) return yield* invalidManifest(manifestPath, 'missing [package] table')

  const name = packageTable.name
  if (typeof name !== 'string' || !isPackageName(name)) {
    return yield* invalidManifest(
      manifestPath,
      'package.name must start with a lowercase letter and contain only lowercase letters, digits, or hyphens',
    )
  }
  const root = packageTable.root
  if (typeof root !== 'string' || root.length === 0) {
    return yield* invalidManifest(manifestPath, 'package.root must be a non-empty string')
  }
  const sourceRoot = packageTable['source-root']
  if (sourceRoot !== undefined && (typeof sourceRoot !== 'string' || sourceRoot.length === 0)) {
    return yield* invalidManifest(manifestPath, 'package.source-root must be a non-empty string')
  }
  return { name, root, sourceRoot }
})

/** Finds the nearest ancestor `silk.toml`, beginning at the selected working directory. */
export const discover = Effect.fn('Project.discover')(function* (
  workingDirectory = '.',
): Effect.fn.Return<string, ProjectError, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const startDirectory = path.resolve(workingDirectory)
  let directory = startDirectory

  while (true) {
    const candidate = path.join(directory, manifestName)
    const exists = yield* fileSystem.exists(candidate).pipe(
      Effect.mapError(
        (cause) =>
          new ProjectError({
            operation: 'Project.discover',
            manifestPath: candidate,
            message: `Cannot inspect ${candidate} while discovering a Silk project`,
            reason: { _tag: 'WrappedFailure', cause },
          }),
      ),
    )
    if (exists) return candidate
    const parent = path.dirname(directory)
    if (parent === directory) {
      return yield* new ProjectError({
        operation: 'Project.discover',
        manifestPath: path.join(startDirectory, manifestName),
        message: `No ${manifestName} found from ${startDirectory}; create one or pass --manifest-path`,
        reason: { _tag: 'ManifestNotFound', startDirectory },
      })
    }
    directory = parent
  }
})

/** Discovers or selects a manifest, validates it, and materializes its canonical root entry. */
export const load = Effect.fn('Project.load')(function* (
  options: LoadOptions = {},
): Effect.fn.Return<Project, ProjectError, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const manifestPath =
    options.manifestPath === undefined
      ? yield* discover(options.workingDirectory)
      : path.resolve(options.workingDirectory ?? '.', options.manifestPath)

  const text = yield* fileSystem.readFileString(manifestPath).pipe(
    Effect.mapError(
      (cause) =>
        new ProjectError({
          operation: 'Project.load',
          manifestPath,
          message: `Cannot read Silk project manifest ${manifestPath}`,
          reason: { _tag: 'WrappedFailure', cause },
        }),
    ),
  )
  const manifest = yield* decodePackage(manifestPath, text)
  const directory = path.dirname(manifestPath)
  const entryPath = path.resolve(directory, manifest.root)
  const selectedSourceRoot =
    manifest.sourceRoot === undefined
      ? path.dirname(entryPath)
      : path.resolve(directory, manifest.sourceRoot)
  const entry = yield* SourceEntry.read(entryPath, selectedSourceRoot).pipe(
    Effect.mapError(
      (error) =>
        new ProjectError({
          operation: 'Project.load',
          manifestPath,
          message: `Cannot load root source for Silk project ${manifest.name}`,
          reason: { _tag: 'InvalidEntry', error },
        }),
    ),
  )

  return Object.freeze({
    _tag: 'Project' as const,
    name: manifest.name,
    manifestPath,
    directory,
    entry,
  })
})
