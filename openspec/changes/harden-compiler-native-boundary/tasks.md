## 1. NativeToolchain boundary

- [x] 1.1 Add `ToolchainError`
- [x] 1.2 Convert `withBuildScope` to `Effect.acquireRelease`
- [x] 1.3 Wrap `runPlanned`/`writeArtifact`/`commit`/`commitCachedArtifact`/`ClangLinker.link` in `Effect.try`
- [x] 1.4 Update `Driver.compile` to `yield*` the toolchain
- [x] 1.5 Add `atomicCommit`
- [x] 1.6 Remove the never-injected `NativeLinker` interface

## 2. Browser-safe barrel and host detection

- [x] 2.1 Move `platform()`/`arch()` out of `Target.ts`
- [x] 2.2 Collapse `Target.triple` into `Target.id`
- [x] 2.3 Bundle smoke check

## 3. Driver artifact hand-off and telemetry

- [x] 3.1 Have `finalizeWasm`/`ClangLinker.link` return `{ path, bytes }`
- [x] 3.2 `HeapObservation` service

## 4. Backend error channels

- [x] 4.1 `WasmBackend.ts:6133` yield `BackendError` instead of throwing `RangeError`
- [x] 4.2 Pin `Backend.emitProgram` to `LlvmError | BackendError`
- [x] 4.3 Convert `emitProgram`/`locate` to `Effect.fnUntraced`

## 5. BootstrapEvaluation host catches

- [x] 5.1 Preserve `cause` in the `HostWrite` and `OsCall` catches in `BootstrapEvaluation.ts`

## 6. Verification

- [x] 6.1 Run `pnpm typecheck` and verify clean
- [x] 6.2 Run `pnpm exec biome check .` and verify clean
- [x] 6.3 Run `pnpm test` and verify the suite passes with new typed-failure and cleanup tests

## 7. Convergence findings

- [x] 7.1 Make build-scope and atomic-commit cleanup non-defecting while preserving the protected exit and removing every staged sibling; add injected cleanup-failure tests
- [x] 7.2 Make `ShimCache` operations Effect-valued with cache-stage `ToolchainError` translation and throwing-cache tests
- [x] 7.3 Make `HeapObservation` required and provide the Node layer at CLI/native application edges plus explicit browser/test layers
