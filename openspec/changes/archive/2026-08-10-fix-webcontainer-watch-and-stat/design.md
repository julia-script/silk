# Design: WebContainer watch support and cheap stat

## Context

See proposal.md — Why. Current state: `WebContainerFileSystem.layer` fails `watch` explicitly;
`stat` locates the entry via the parent's `readDirectory` but then reads the whole file to report
a byte size, and `exists`/`access` route through `stat`, inheriting the content read. The upstream
`@webcontainer/api` exposes `fs.watch(path, options?, listener)` returning a watcher with
`close()`, with Node-style `'rename' | 'change'` event types and an optional `{ recursive }`
option. The package already has a proven pattern for adapting synchronous callback subscriptions
into per-consumer streams (`WebContainerEvent.stream` over `Stream.callback` with an unbounded
queue).

## Goals / Non-Goals

**Goals**

- `FileSystem.watch` works for files and directories, with per-consumer lifecycle tied to the
  stream scope.
- No filesystem query (`stat`, `access`, `exists`, `watch` classification) reads file contents.
- The primitive `WebContainer.FileSystem` interface exposes watch so non-Effect-FileSystem
  consumers (and tests) can reach it.

**Non-Goals**

- Accurate file sizes in `stat` (upstream exposes no stat; sizes become a documented neutral `0`).
- Debouncing, coalescing, or recursive-tree diffing on top of native events — consumers layer
  their own policies.
- Backpressure for watch events (same documented contract as the existing event streams).

## Decisions

1. **Primitive shape**: add `watch(path, options?: { recursive?: boolean })` to
   `WebContainer.FileSystem`, returning the package's own `Stream` of raw
   `{ event: 'rename' | 'change', filename }` notifications with `WebContainerError` failures.
   Reuse the `Stream.callback` + `acquireRelease(subscribe, close)` pattern from
   `WebContainerEvent.stream` rather than inventing a second adapter. Alternative considered:
   exposing the raw watcher object — rejected, it would leak an unmanaged external handle across
   the boundary, which the package's own README forbids.

2. **Event mapping in the standard service**: `'change'` maps to `Update`. `'rename'` is
   ambiguous between create and remove in Node semantics, so the implementation resolves it by
   listing the parent directory of the affected path: present → `Create`, absent → `Remove`.
   Listing is O(entries) and reads no contents, keeping the spec's no-content-read rule.
   Alternative considered: emitting `Update` for everything — rejected, tree views need
   create/remove to stay correct without full rescans.

3. **Missing-path failure**: `watch` verifies the target exists (parent listing) before
   registering the watcher so a missing path fails `NotFound` immediately, matching the spec
   scenario, instead of surfacing an opaque upstream error asynchronously.

4. **stat without content reads**: `stat` keeps its parent-listing lookup for type but reports
   `FileSystem.Size(0)` for files, same as directories. `exists`/`access` keep routing through
   `stat`, which now makes them listing-only automatically. `truncate` and `copy` still read
   contents — they are content operations, not queries. Alternative considered: caching sizes on
   write — rejected as stateful complexity the consumer (a file tree) does not need.

5. **Test double**: `test/support/VirtualFileSystem.ts` gains a minimal watcher registry
   (register/close, manual `emit` helper) so watch mapping and lifecycle are testable without a
   browser; the Playwright browser suite gets one end-to-end watch test where a spawned process
   writes a file.

## Risks / Trade-offs

- [Upstream `watch` types are loose (`filename: string | Uint8Array`, no recursive guarantee
  across paths)] → normalize filenames to strings at the boundary and test directory and nested
  cases explicitly in the browser suite.
- [`rename` disambiguation races: an entry can be recreated between the event and the parent
  listing] → acceptable; the emitted classification is eventually consistent and tree consumers
  reconcile on the next event. Documented in the README.
- [Size `0` breaks consumers wanting real sizes] → package is unreleased; README compatibility
  table states sizes are approximated and `readFile(...).byteLength` is the accurate path.

## Migration Plan

Single package, unreleased: implement, update README and changeset, land with the existing
`julia/platform-webcontainer` branch before first publish. No rollback machinery needed.

## Open Questions

- Whether the IDE will want a debounced, coalesced watch layer — deferred to the IDE change; it
  composes on top of this stream without touching this package.
