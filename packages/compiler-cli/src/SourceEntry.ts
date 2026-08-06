import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'

/**
 * The CLI's single-file entry point into a compilation request. The compiler resolves imports
 * only among the sources it is handed, and its module identities are canonical — no dots, no
 * path separators at the edges. A file path is therefore not a module identity: this actor owns
 * the one translation between them, so the rest of the CLI never invents module names.
 *
 * Multi-file resolution is deliberately absent. Until the compiler resolves imports against a
 * filesystem, one file is the honest scope, and an import in that file will be reported as an
 * unknown module rather than silently searched for on disk.
 */

/** One source file read from disk, paired with the canonical module identity derived from it. */
export interface SourceEntry {
  readonly _tag: 'SourceEntry'
  readonly module: string
  readonly path: string
  readonly bytes: Uint8Array
}

/** The file could not be read, or its name yields no canonical module identity. */
export class SourceEntryError extends Data.TaggedError('SourceEntryError')<{
  readonly operation: 'SourceEntry.read' | 'SourceEntry.identify'
  readonly path: string
  readonly message: string
  readonly reason:
    | { readonly _tag: 'InvalidIdentity' }
    | { readonly _tag: 'WrappedFailure'; readonly cause: unknown }
}> {}

/**
 * Derives a canonical module identity from a file's base name by dropping every extension.
 * Mirrors `ModuleClosure`'s canonical-identity rule; a name that cannot satisfy it is rejected
 * here rather than thrown as a `RangeError` from deep inside the driver.
 */
export const identify = (baseName: string): string | undefined => {
  const stem = baseName.split('.').at(0) ?? ''
  return /^[A-Za-z0-9_-]+$/.test(stem) ? stem : undefined
}

/** Reads one source file and pairs it with its canonical module identity. */
export const read = Effect.fn('SourceEntry.read')(function* (
  file: string,
): Effect.fn.Return<SourceEntry, SourceEntryError, FileSystem.FileSystem | Path.Path> {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path

  const bytes = yield* fileSystem.readFile(file).pipe(
    Effect.mapError(
      (cause) =>
        new SourceEntryError({
          operation: 'SourceEntry.read',
          path: file,
          message: `Cannot read source file ${file}`,
          reason: { _tag: 'WrappedFailure', cause },
        }),
    ),
  )

  const baseName = path.basename(file)
  const module = identify(baseName)
  if (module === undefined) {
    return yield* new SourceEntryError({
      operation: 'SourceEntry.identify',
      path: file,
      message: `File name ${baseName} does not yield a module identity; use letters, digits, underscores, or hyphens`,
      reason: { _tag: 'InvalidIdentity' },
    })
  }

  return Object.freeze({ _tag: 'SourceEntry' as const, module, path: file, bytes })
})
