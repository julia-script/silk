## Why

The compiler's native-toolchain boundary violates the effect-patterns boundary rule. `NativeToolchain.ts` is a fully synchronous imperative core (raw `node:child_process`/`node:crypto`/`node:fs`/`node:os`/`node:path` with scattered try/catch and a manual try/finally in `withBuildScope`) that returns data-union failures, then gets driven synchronously inside `Driver.compile`'s `Effect.gen`. A spawn/fs throw therefore escapes the generator as a Defect rather than a typed error, and the same fs failure lands in two different channels depending on which line threw. `node:*` ownership is split three ways, `Target.ts` imports `node:os` and is re-exported from the barrel the design claims is browser-safe, `Driver.ts` re-reads artifacts from disk with raw `readFileSync`, and `WasmBackend` throws a `RangeError` inside an `Effect.gen` instead of yielding a typed `BackendError`.

## What Changes

- **`NativeToolchain` becomes a thin Effect boundary**: `Effect.try` around spawn/fs, `Effect.acquireRelease` for build scopes and temp dirs, a `ToolchainError` family in the error channel; `ToolchainPlan` stays pure.
- **One `atomicCommit` seam** (guaranteed cleanup) replaces four divergent copy-paste copies and fixes the stale-temp leak in `makeDiskArtifactCache.set`.
- **`node:os` host detection moves out of `Target`** into a Node-only boundary, so importing the package root no longer pulls node built-ins; the redundant `Target.triple` field is collapsed.
- **`Driver` stops re-reading artifacts** with `readFileSync` (finalizers return bytes); `node:fs`/memory telemetry become single-owner.
- **`WasmBackend`** routes request-validation failures to `BackendError` instead of throwing; **`Backend.emitProgram`** pins its error channel with `catchTag` rather than `_tag`-string probing and becomes `Effect.fnUntraced` with a concrete debug-location type.
- **`BootstrapEvaluation`** host-provider catches preserve the cause instead of flattening to `'Other'`.
- **Remove the never-injected `NativeLinker` interface** (single implementation, never provided).

## Capabilities

### New Capabilities

<!-- none -->

### Modified Capabilities

- `bootstrap-native-toolchain`: typed failure channel, atomic artifact/cache commit with cleanup, browser-safe barrel, in-memory artifact hand-off.
- `bootstrap-backend`: expected request-validation failures yield typed `BackendError`, never defects.

## Impact

Observable: native-toolchain failures now surface as typed errors (callers can recover on stage and cause), and invalid backend requests no longer bypass `BackendError`. Everything else is internal refactoring. Node-only code stays a deep import; `@silklang/compiler` root becomes genuinely browser-safe.
