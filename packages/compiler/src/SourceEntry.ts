import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as SourceResolver from './SourceResolver.js'

/**
 * A project's explicit root source. A physical path is not a module identity: this actor normalizes
 * the selected source root, verifies that the entry is below it, and derives the canonical
 * extensionless identity used by the compiler.
 */

/** One source file read from disk, paired with the canonical module identity derived from it. */
export interface SourceEntry {
  readonly _tag: 'SourceEntry'
  readonly module: string
  readonly path: string
  readonly sourceRoot: string
  readonly bytes: Uint8Array
}

/** The file could not be read, or its name yields no canonical module identity. */
export class SourceEntryError extends Data.TaggedError('SourceEntryError')<{
  readonly operation: 'SourceEntry.read' | 'SourceEntry.identify'
  readonly path: string
  readonly message: string
  readonly reason:
    | { readonly _tag: 'InvalidIdentity' }
    | { readonly _tag: 'OutsideSourceRoot'; readonly sourceRoot: string }
    | { readonly _tag: 'WrappedFailure'; readonly cause: unknown }
}> {}

/**
 * Derives a canonical module identity from a relative file path by dropping its exact `.silk`
 * suffix.
 * Mirrors `ModuleClosure`'s canonical-identity rule; a name that cannot satisfy it is rejected
 * here rather than thrown as a `RangeError` from deep inside the driver.
 */
export const identify = (relativePath: string, separator = '/'): string | undefined => {
  if (!relativePath.endsWith('.silk')) return undefined
  const extensionless = relativePath.slice(0, -'.silk'.length)
  const module = extensionless.split(separator).join('/')
  return SourceResolver.isCanonicalModule(module) ? module : undefined
}

/** Reads one source file and pairs it with its canonical module identity. */
export const read = Effect.fn('SourceEntry.read')(function* (
  file: string,
  selectedSourceRoot?: string,
): Effect.fn.Return<SourceEntry, SourceEntryError, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path

  const absoluteFile = path.resolve(file)
  const sourceRoot = path.resolve(selectedSourceRoot ?? path.dirname(absoluteFile))
  const relative = path.relative(sourceRoot, absoluteFile)
  if (relative === '..' || relative.startsWith(`..${path.sep}`) || path.isAbsolute(relative)) {
    return yield* new SourceEntryError({
      operation: 'SourceEntry.identify',
      path: absoluteFile,
      message: `Source entry ${absoluteFile} is outside source root ${sourceRoot}`,
      reason: { _tag: 'OutsideSourceRoot', sourceRoot },
    })
  }
  const module = identify(relative, path.sep)
  if (module === undefined) {
    return yield* new SourceEntryError({
      operation: 'SourceEntry.identify',
      path: absoluteFile,
      message: `Source entry ${absoluteFile} must be a canonical relative path ending in .silk`,
      reason: { _tag: 'InvalidIdentity' },
    })
  }

  const bytes = yield* fileSystem.readFile(absoluteFile).pipe(
    Effect.mapError(
      (cause) =>
        new SourceEntryError({
          operation: 'SourceEntry.read',
          path: absoluteFile,
          message: `Cannot read source file ${absoluteFile}`,
          reason: { _tag: 'WrappedFailure', cause },
        }),
    ),
  )

  return Object.freeze({
    _tag: 'SourceEntry' as const,
    module,
    path: absoluteFile,
    sourceRoot,
    bytes: Uint8Array.from(bytes),
  })
})
