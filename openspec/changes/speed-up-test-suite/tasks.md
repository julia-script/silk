## 1. Baseline

- [x] 1.1 Run `vitest run --reporter=verbose` on `packages/compiler` (built workspace, quiet machine), save the per-file duration ranking into the change directory as `baseline.md`
- [x] 1.2 Confirm the ranking matches the spike's top offenders (`LexerPressure`, `TemporaryDirectoryAcceptance`, `StackVmPressure`, determinism family); adjust deletion order if it doesn't

## 2. Determinism consolidation (spec: bootstrap-compiler-driver)

- [x] 2.1 Pick the third canary: verify `LlvmWasmDeterminism`, `ConditionalConformanceDeterminism`, and the chosen stored-callable determinism file together cover native+wasm release backends, stdlib imports, generics, and callable environments; extend a canary's program if a surface is missing
- [ ] 2.2 Verify each of the other 20 `*Determinism.test.ts` files' feature areas keep in-process golden byte-comparisons in their remaining test files; add a golden assertion where one is missing
- [ ] 2.3 Delete the 20 `*Determinism.test.ts` files and their `fixtures/*-determinism.mjs` fixtures in one commit
- [ ] 2.4 Mutation spot-check (design D7): for 2 sampled deleted files, locally re-introduce a representative nondeterminism/regression and confirm a surviving test fails; record the check in the PR description

## 3. Native legs fold into the corpus (spec: bootstrap-compiler-driver)

- [ ] 3.1 Inventory the ~100 `Driver.compile` sites in feature files; classify each against the design D2 allowlist (target-specific stays, exit-code parity folds)
- [ ] 3.2 Move each foldable program into `test/support/corpus.ts` and delete its standalone native leg, one commit per file family (RuntimeSlice, StoredCallable, String, EffectSuspension wasm-dupes, trivial-syntax acceptance files, ConditionalConformance)
- [ ] 3.3 Delete whole-file duplicates (`RuntimeSliceNative.test.ts`, `RuntimeSliceAcceptance` engine dupes, wasm-leg repeats) where the identical program remains covered
- [ ] 3.4 Downgrade link-only tests (`StoredCallableRuntime`, `StoredCallableDiagnostic`, `OpaqueRepresentationEngines`): assert `Analysis.codegen` + existing `opt -passes=verify`, keep one retained link test
- [ ] 3.5 Run `DriverNativeAcceptance` and confirm every folded program executes and agrees; confirm corpus runtime grew by ~0.8s per added program, not more

## 4. Pressure-sweep tiering (spec: bootstrap-language-pressure-programs)

- [x] 4.1 Rework `LexerPressure.test.ts` failure-ordinal sweep: evaluator+wasm every ordinal, native at first-failure / mid-growth / completion ordinals only
- [x] 4.2 Same tiering for `StackVmPressure.test.ts`
- [x] 4.3 Remove the quota-allocator variants duplicating `OwnedAllocation*` coverage and the "general MIR operations" duplicate case
- [x] 4.4 Re-run both files; confirm each drops below 30s and their 120s+ explicit timeouts can be lowered

## 5. Perf assertions out of the correctness suite

- [x] 5.1 Trim `SynchronousEffectCost.test.ts` to its structural normalization assertions (spec: entry structure only, no byte/branch/timing counts)
- [x] 5.2 Delete `OccurrencePerformance.test.ts`
- [ ] 5.3 Replace exact diagnostic-message-string assertions with code+span assertions in the 12 affected files (catalog gates wording via `documentation:check`)
- [ ] 5.4 Prune the ~8–10 `EditorIntelligence.test.ts` cases duplicated by `DeclarationIndex.test.ts`

## 6. Native artifact disk cache (spec: bootstrap-native-toolchain)

- [x] 6.1 Make `defaultArtifactCache()` in `packages/compiler/src/NativeToolchain.ts` return `makeDiskArtifactCache(SILK_NATIVE_CACHE_DIR)` when the variable is set; add clang version to the cache key; recompile on corrupt/missing entries
- [x] 6.2 Add a test: two child processes with the same cache dir — second compile produces a byte-identical artifact without invoking clang (assert via a counting wrapper or phase report)
- [x] 6.3 Correct the `packages/compiler/vitest.config.ts` comment (key includes clang identity, not merely "Clang version"); clear the stale pre-refactor `~/.cache/silk-effect/native` contents note in the PR
- [ ] 6.4 Full compiler suite twice back-to-back; second run shows clang-phase time near zero in phase reports

## 7. CI and worktree caches

- [x] 7.1 Add `actions/cache` for `.turbo` (key: lockfile + turbo config hash, with restore-keys prefix fallback) and for the native cache dir in `.github/workflows/ci.yml`
- [x] 7.2 In `scripts/turbo.mjs`, set `TURBO_CACHE_DIR` to the main checkout's shared cache when running under `.claude/worktrees/*`
- [ ] 7.3 Verify: push a no-op commit, confirm CI test tasks replay from turbo cache; run `pnpm test` in a fresh worktree, confirm cache hits

## 8. AGENTS.md rules and close-out

- [x] 8.1 Add the "Keep tests cheap" section to AGENTS.md: cheapest-tier proof obligation, corpus-first native coverage with the D2 allowlist, no per-feature determinism tests, no timing/byte-count assertions, one Analysis snapshot per program per file, prefer corpus/table cases over new files
- [ ] 8.2 Re-run the reporter baseline from 1.1, diff against `baseline.md`, record the before/after totals in the change
- [ ] 8.3 Confirm target met (compiler-suite CPU down ≥35%); if short, consult the follow-up list (snapshot sharing, stdlib memoization) rather than re-adding scope here
