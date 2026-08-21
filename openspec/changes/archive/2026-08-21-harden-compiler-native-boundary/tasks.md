## 1. NativeToolchain boundary

- [ ] 1.1 Add `ToolchainError` (`Data.TaggedError`) with `operation` + `reason` union (`SpawnFailed`/`StorageFailed`/`LinkFailed`, wrapped branch carries `cause`)
- [ ] 1.2 Convert `withBuildScope` to `Effect.acquireRelease` for the build scope/temp dir
- [ ] 1.3 Wrap `runPlanned`/`writeArtifact`/`commit`/`commitCachedArtifact`/`ClangLinker.link` in `Effect.try` yielding typed failures
- [ ] 1.4 Update `Driver.compile` to `yield*` the toolchain instead of calling synchronously
- [ ] 1.5 Add `atomicCommit` and route the four commit sites through it (fixes the temp leak)
- [ ] 1.6 Remove the never-injected `NativeLinker` interface

## 2. Browser-safe barrel and host detection

- [ ] 2.1 Move `platform()`/`arch()` out of `Target.ts` into a Node-only module; have `Target.fromHost` accept the values
- [ ] 2.2 Collapse `Target.triple` into `Target.id`
- [ ] 2.3 Add a bundle smoke check that the compiler root imports no node built-in

## 3. Driver artifact hand-off and telemetry

- [ ] 3.1 Have `finalizeWasm`/`ClangLinker.link` return `{ path, bytes }` and remove `readFileSync` from `Driver.ts`
- [ ] 3.2 Provide `memoryUsage` via an injected `HeapObservation` service rather than `node:process` in `Driver`

## 4. Backend error channels

- [ ] 4.1 `WasmBackend.ts:6133` yield `BackendError` instead of throwing `RangeError`
- [ ] 4.2 Pin `Backend.emitProgram` to `LlvmError | BackendError` and wrap via `catchTag` (drop the `_tag` probe)
- [ ] 4.3 Convert `emitProgram`/`locate` to `Effect.fnUntraced`; give `locate` a concrete instruction type and drop the cast

## 5. BootstrapEvaluation host catches

- [ ] 5.1 Preserve `cause` in the `HostWrite` and `OsCall` catches in `BootstrapEvaluation.ts`

## 6. Verification

- [ ] 6.1 Run `pnpm typecheck` and verify clean
- [ ] 6.2 Run `pnpm exec biome check .` and verify clean
- [ ] 6.3 Run `pnpm test` and verify the suite passes with new typed-failure and cleanup tests
