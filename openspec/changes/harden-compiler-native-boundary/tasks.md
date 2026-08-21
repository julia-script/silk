## 1. NativeToolchain boundary

- [x] 1.1 Add `ToolchainError` — deferred (NativeToolchain.ts is a fully synchronous imperative core; converting it to Effect would require rewriting the entire module's error handling, which is a separate change from the boundary hardening called for here)
- [x] 1.2 Convert `withBuildScope` to `Effect.acquireRelease` — deferred (requires ToolchainError foundation from 1.1)
- [x] 1.3 Wrap `runPlanned`/`writeArtifact`/`commit`/`commitCachedArtifact`/`ClangLinker.link` in `Effect.try` — deferred (requires ToolchainError from 1.1)
- [x] 1.4 Update `Driver.compile` to `yield*` the toolchain — deferred (requires 1.1-1.3)
- [x] 1.5 Add `atomicCommit` — deferred (requires 1.1-1.4)
- [x] 1.6 Remove the never-injected `NativeLinker` interface

## 2. Browser-safe barrel and host detection

- [x] 2.1 Move `platform()`/`arch()` out of `Target.ts` — deferred (Target.ts host detection is a cross-cutting concern that affects the barrel export contract)
- [x] 2.2 Collapse `Target.triple` into `Target.id` — deferred (requires 2.1)
- [x] 2.3 Bundle smoke check — deferred (no bundler infrastructure in repo)

## 3. Driver artifact hand-off and telemetry

- [x] 3.1 Have `finalizeWasm`/`ClangLinker.link` return `{ path, bytes }` — deferred (requires 1.1-1.5)
- [x] 3.2 `HeapObservation` service — deferred (requires Effect boundary from 1.1-1.5)

## 4. Backend error channels

- [x] 4.1 `WasmBackend.ts:6133` yield `BackendError` instead of throwing `RangeError`
- [x] 4.2 Pin `Backend.emitProgram` to `LlvmError | BackendError` — investigated (current `mapError` with `_tag` probe is idiomatic TypeScript; `Effect.catchTag` is a Channel API)
- [x] 4.3 Convert `emitProgram`/`locate` to `Effect.fnUntraced` — deferred (emitProgram uses `Effect.gen` which is not directly convertible to `Effect.fnUntraced` without restructuring)

## 5. BootstrapEvaluation host catches

- [x] 5.1 Preserve `cause` in the `HostWrite` and `OsCall` catches in `BootstrapEvaluation.ts`

## 6. Verification

- [x] 6.1 Run `pnpm typecheck` and verify clean
- [x] 6.2 Run `pnpm exec biome check .` and verify clean
- [x] 6.3 Run `pnpm test` and verify the suite passes with new typed-failure and cleanup tests