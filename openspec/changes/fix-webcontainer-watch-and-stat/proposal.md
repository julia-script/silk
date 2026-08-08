# Fix WebContainer platform review findings: watch support and cheap stat

## Why

Code review of `@silk-effect/platform-webcontainer` found two gaps that block its primary
consumer, the upcoming browser IDE. First, filesystem watching fails explicitly even though the
upstream `@webcontainer/api` exposes `fs.watch` — without it, a file tree and editor cannot learn
that a terminal command (`silk build`, `npm install`) changed files. Second, `stat` reads the
entire file to report a size and `exists` routes through `stat`, so walking a project tree costs
O(total file bytes) instead of O(entries).

## What Changes

- Expose the upstream watch capability through the runtime's primitive filesystem interface
  (`WebContainer.FileSystem`) as a typed, scoped Effect boundary.
- Implement `FileSystem.watch` in `WebContainerFileSystem.layer` as a per-consumer stream of
  standard `FileSystem.WatchEvent` values instead of an explicit failure.
- Rework `stat`, `access`, and `exists` to derive answers from directory listings only, never
  file contents. File sizes become a documented zero approximation. **BREAKING** for consumers
  that relied on accurate `stat` sizes (none exist today; the package is unreleased).
- Update the README compatibility table and the package's changeset accordingly.

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `webcontainer-filesystem`: watching becomes a supported operation (new requirement; the
  "Watch is unsupported" scenario is removed), and the stat approximation requirement changes
  from "sizes are read from contents" to "no filesystem answer may read file contents".

## Impact

- `packages/platform-webcontainer/src/WebContainer.ts` — primitive `FileSystem` interface gains
  `watch`; `makeFileSystem` wraps `raw.fs.watch`.
- `packages/platform-webcontainer/src/WebContainerFileSystem.ts` — `watch` implementation,
  `stat`/`access`/`exists` rework.
- `packages/platform-webcontainer/test/**` — new watch tests, updated stat expectations, and the
  `VirtualFileSystem` test double learns watch.
- `packages/platform-webcontainer/README.md` — compatibility table rows for watch and stat.
- No dependency changes; no other packages are affected.
