/**
 * Deterministic storage failures for the format tests.
 *
 * The failure paths these tests cover — a source file that cannot be read, an atomic commit that
 * cannot be completed — used to be induced with `chmod(path, 0o000)` and `chmod(directory, 0o555)`.
 * That works for an unprivileged user and silently does nothing for root, which bypasses the file
 * permission check entirely: the read succeeds, the rename succeeds, and the assertion that wanted
 * `Failed` sees `Changed`. Issue #159 records the cost — a clean checkout is red in any root
 * container, and every contributor working in one pays to rediscover that the red is not theirs.
 *
 * Injecting the failure at the `FileSystem` seam removes the dependency on who is running the
 * suite. The workflow still takes the same branch it takes when the operating system denies the
 * operation for real, because it reaches that branch through the same `PlatformError` the platform
 * layer would have produced. The precondition is now expressible under every UID rather than only
 * under some, so the coverage is the same everywhere instead of silently better on CI.
 *
 * Only the two operations the failure paths depend on are overridden, and only for paths named by
 * the caller. Every other call — including the reads a test makes to assert that a file was left
 * untouched — goes to the real filesystem.
 */
import * as Effect from 'effect/Effect'
import type * as FileSystem from 'effect/FileSystem'
import * as PlatformError from 'effect/PlatformError'

export interface Denials {
  /** Canonical paths whose `readFile` fails, standing in for a source file the process cannot read. */
  readonly readFile?: ReadonlyArray<string>
  /** Canonical destination paths whose `rename` fails, standing in for an atomic commit that cannot land. */
  readonly rename?: ReadonlyArray<string>
}

const denied = (method: string, syscall: string, path: string): PlatformError.PlatformError =>
  PlatformError.systemError({
    _tag: 'PermissionDenied',
    module: 'FileSystem',
    method,
    syscall,
    pathOrDescriptor: path,
    description: `Injected permission denial for ${path}`,
  })

/**
 * Wraps a filesystem so the named operations fail with `PermissionDenied` on the named paths.
 *
 * Paths are compared exactly, so callers pass canonical paths: the workflow resolves its selection
 * through `realPath` before touching anything, and a temporary directory is a symlink on some
 * platforms.
 */
export const denying = (
  fileSystem: FileSystem.FileSystem,
  denials: Denials,
): FileSystem.FileSystem => ({
  ...fileSystem,
  readFile: (path) =>
    denials.readFile?.includes(path)
      ? Effect.fail(denied('readFile', 'open', path))
      : fileSystem.readFile(path),
  rename: (oldPath, newPath) =>
    denials.rename?.includes(newPath)
      ? Effect.fail(denied('rename', 'rename', newPath))
      : fileSystem.rename(oldPath, newPath),
})
