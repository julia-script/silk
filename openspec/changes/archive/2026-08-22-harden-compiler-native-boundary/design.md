## Context

See proposal.md. The boundary sits between `NativeToolchain.ts` (Node, deep import), `Driver.ts` / `Pipeline.ts` (Effect orchestration), `Target.ts` (pure data), `Backend.ts` / `WasmBackend.ts` (Effect backends), and `BootstrapEvaluation.ts` (synchronous leaf).

## Goals / Non-Goals

**Goals:** one owner per external dependency; expected failures in the typed error channel; temp-dir/scope cleanup via `acquireRelease`; a genuinely browser-safe root barrel.

**Non-Goals:** changing build-plan semantics; renaming public subpaths; god-module splits (separate changes).

## Decisions

### NativeToolchain service boundary

Split `NativeToolchain` into a pure planning surface (existing `ToolchainPlan`) and an effectful executor. Add a `ToolchainError extends Data.TaggedError` with `operation`, `message`, and a `reason` union (`SpawnFailed`, `StorageFailed`, `LinkFailed`) where the wrapped branch carries `cause`. `withBuildScope` becomes `Effect.acquireRelease(mkdtemp, rmSync)`. `runPlanned`/`writeArtifact`/`commit`/`commitCachedArtifact`/`ClangLinker.link` all `yield*` typed failures. `Driver.compile` `yield*`s them instead of calling synchronously.

### One atomic-commit seam

Add `atomicCommit(destination, bytes, { mode? })`: mkdir, write temp sibling (`${path}.silk-tmp-${pid}`), `renameSync`, and `rmSync` the temp in a finally-like bracket on any failure. Route `makeDiskArtifactCache.set` (104), `commitCachedArtifact` (192), `commit` (322), and `ClangLinker.link` (458) through it. This fixes the leak where `makeDiskArtifactCache.set`'s catch did not remove its temp.

### Host detection leaves Target

Move `platform()`/`arch()` into `NativeToolchain` (or a small `HostTarget.ts` next to it) and have `Target.fromHost` accept the values instead of reading `node:os`. `Target.ts` becomes node-free. Collapse `Target.triple` into `Target.id` (they are always equal).

### Driver artifact hand-off

Have `finalizeWasm` / `ClangLinker.link` return `{ path, bytes }` so `Driver` seeds the artifact cache without `readFileSync`. Provide `memoryUsage` through an injected `HeapObservation` service (default Node impl at the app edge) instead of importing `node:process` in `Driver`.

### Backend error channels

`WasmBackend.ts:6133` returns `yield* new BackendError({ reason: { _tag: 'InvalidMir', ... } })` instead of throwing. `Backend.emitProgram` becomes `Effect.fnUntraced` pinned to `LlvmError | BackendError`; `LlvmBackend.emit` wraps via `patchErrors`/`catchTag` on `LlvmError` rather than `cause._tag === 'BackendError'` probing. `locate` takes the concrete instruction type (the return of `Value.instruction`) and drops the `unknown` cast.

### BootstrapEvaluation host catches

`HostWrite` (`BootstrapEvaluation.ts:2912`) and the `OsCall` arm (`:3342`) keep the typed-result path for expected failures and reserve the catch for unexpected throws, mapping them with `cause` preserved rather than a bare `'Other'`.

### Remove NativeLinker

Delete the `NativeLinker` interface; `Driver` calls the `ClangLinker` implementation directly (single implementation today).

## Risks / Trade-offs

- [Error channel change is observable] → new spec scenarios pin the typed-failure behavior; test replacements verify `cause`/stage.
- [Browser-safe barrel] → a bundle smoke check asserts no `node:` module in the root import graph.
- [Atomic commit behavior] → the determinism/durable-cache tests plus a new "failed rename leaves no temp" case.

## Validation

`pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`; new `@effect/vitest` cases for spawn/storage/link typed failures, temp cleanup, and the wasm invalid-request `BackendError`.
