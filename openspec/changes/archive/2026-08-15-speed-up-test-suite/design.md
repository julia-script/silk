# Design

## Context

See proposal.md — Why. Measured facts this design relies on (six spikes, 2026-08-15):

- One native `Driver.compile` ≈ 1.05s = 64% JS LLVM-bitcode backend + 24% frontend + 11% clang + <1% binary execution. Native tests are slow because of the extra JS pipeline, not the toolchain.
- Clang-touching test files: 66 of 225, ~70% of per-file wall. Fresh-process determinism files: 24, ~209s. Vitest harness: ~4–5% of suite CPU. Slowest files: `LexerPressure` 226s (162s in one failure-ordinal sweep), `TemporaryDirectoryAcceptance` 193s, `StackVmPressure` 115s.
- `DriverNativeAcceptance.test.ts` already differential-tests interpreter vs native over a ~70-program corpus (`test/support/corpus.ts`) at ~0.77s/program, serially.
- `NativeToolchain.makeDiskArtifactCache` exists and works (key = sha256 of kind/triple/profile/clang/shim/bitcode; measured hit: 1.76s → 4ms for the toolchain step); `defaultArtifactCache()` is a process-local `Map`; `SILK_NATIVE_CACHE_DIR` is set by `packages/compiler/vitest.config.ts` and read by nothing.
- Ruled out by measurement: bun (2x slower on compiler JS, incompatible with `@effect/vitest`), plain-script conversion (~4–5% ceiling, loses timeouts/isolation), `--no-isolate` (no wall change), clang `-O0` (no-op), clang batching (clang is ~2%).

## Goals / Non-Goals

**Goals**
- Cut compiler-suite CPU ~35–50% by deleting redundant tests and tiering expensive legs, without losing any distinct failure mode the suite can currently catch.
- Make determinism and native-agreement coverage *centralized* (canaries + corpus) so the marginal cost of a new language feature's tests is one corpus entry, not a new clang-spawning file.
- Stop regrowth: encode the cost rules where AI agents read them (AGENTS.md).
- Warm caches everywhere a run can be warm (CI, worktrees, cross-process native artifacts).

**Non-Goals**
- No `Analysis` snapshot sharing or stdlib elaboration memoization (compiler-side; follow-up change).
- No harness replacement (vitest stays), no worker/pool retuning beyond what exists.
- No behavior change to the compiler beyond the opt-in disk cache default.

## Decisions

**D1. Determinism: 3 canaries + in-process goldens, not 23 fresh-process files.**
Fresh-process determinism catches nondeterminism whose source is process-local state (map iteration order, hashing seeds, pointer-derived ordering). That class is global to the compiler, not per-feature: any sufficiently rich program that exercises the full artifact surface will surface it. Keep `ScannerDeterminism` (stdlib imports, allocation, both release backends, HIR/ownership/MIR encodings, 7.5s), `ConditionalConformanceDeterminism` (generics, conformance memo order, both backends, 1.0s), and `StoredCallableDeterminism` (callable environments, native+wasm execution, 38.6s). `LlvmWasmDeterminism` (baseline pick) measured as a trivial-identity program whose whole surface the other canaries subsume — it is deleted with the rest. Every deleted file's per-feature byte-identity claim remains enforced by its committed-golden comparisons, which run in-process. Alternative considered: keep all 23 but share one spawned process per file — rejected, still pays 2 full release pipelines × 20 files for no added failure mode.

**D2. Native agreement: corpus-only, with an explicit target-specific allowlist.**
`DriverNativeAcceptance` is already the designated differential gate. Feature files' programs move into `test/support/corpus.ts`; their standalone `Driver.compile` + `spawnSync` legs are deleted. Native legs stay *only* where lowering is genuinely target-specific and the corpus's exit-code differential cannot express the claim: `EffectSuspensionNative`, `DropHookExecution`, `RecursionStackBoundary`, syscall-touching tests (`OsFileSystem`, `TemporaryDirectoryAcceptance`, `HostInput`, `StandardStreams`), and allocation-metrics tests. The allowlist is written down in AGENTS.md so the burden of proof is on adding a native leg, not removing one.

**D3. Failure-ordinal sweeps: evaluator+wasm carry every ordinal, native carries boundaries.**
The sweep's claim (typed `OutOfMemory`, exactly-once release, no partial exposure) is a semantics claim the evaluator and wasm engines check cheaply per ordinal. Native's unique contribution is leak evidence through the real allocator — preserved at first-failure, one mid-growth, and completion ordinals. This converts O(ordinals) native pipelines into O(3) per pressure program and removes the two worst single tests in the suite. The quota constant currently embedded in source per iteration also defeats the artifact cache; boundary-only native legs make that moot.

**D4. Disk cache: flip the default, don't touch 66 call sites.**
`defaultArtifactCache()` in `packages/compiler/src/NativeToolchain.ts` returns `makeDiskArtifactCache(process.env.SILK_NATIVE_CACHE_DIR)` when the variable is set, else the existing Map. Every existing `Driver.compile` caller inherits it; the vitest config env line becomes live as originally intended. Two known limitations, accepted and documented: the key hashes the clang *path* (stale after a clang upgrade at the same path — mitigated by including clang version in the key while we're there), and no eviction (mitigated: CI cache is bounded by the actions/cache limit; local dir is small — 22MB after weeks of the old spike). Honest sizing: a hit skips only the clang step (~11% of a compile) plus the shim compile, so this is a small steady win, not the headline.

**D5. CI/worktree caches.**
`actions/cache` on `.turbo` keyed by lockfile+turbo config hash with restore-keys fallback, and on `SILK_NATIVE_CACHE_DIR`. Worktrees: `scripts/turbo.mjs` sets `TURBO_CACHE_DIR` to a repo-adjacent shared path when the checkout is under `.claude/worktrees/` (turbo hashes are repo-relative, so cross-worktree hits are sound).

**D6. Perf assertions leave the correctness suite.**
`SynchronousEffectCost` keeps only its *structural* assertions (entry structure omits foldable constructor calls — spec-mandated) and drops exact byte/branch counts and timing rounds; `OccurrencePerformance` is deleted (timed rounds on shared CI are a flake generator, and the spec makes no performance claim).

**D7. Verification is measured, not asserted.**
Before the deletion PR: one `vitest run --reporter=verbose` baseline, committed to the change as a ranking. After each phase: same run, diff the totals. Deletions must also pass a mutation-style spot check: for 3 sampled deleted files, re-introduce a representative historical bug (or revert its fixing commit locally) and confirm a surviving test still fails.

## Risks / Trade-offs

- [Deleting a test that was the only guard for a real regression] → D7's spot check; deletions grouped by pattern in separate commits so `git revert` restores a whole family; corpus entries land in the same commit as the leg they replace.
- [Canary set misses a feature-specific nondeterminism source] → canaries chosen to cover the full artifact surface (both backends, stdlib, generics, callables); goldens still catch any in-process nondeterminism per feature; a future nondeterminism escape adds a canary, not 20 files.
- [Boundary-ordinal native sweep misses a native-only leak at an interior ordinal] → interior ordinals still checked by wasm (linear memory) and evaluator (logical releases); native allocator behavior does not vary by ordinal index, only by rollback path shape, and the boundary set covers all three path shapes (immediate failure, partial init, full success).
- [Disk cache serves a stale artifact after toolchain change] → add clang version to the key in the same change; corrupted/missing entries recompile per the spec delta.
- [Turbo cache sharing across worktrees races concurrent runs] → turbo's cache writes are atomic (content-addressed files); worst case is a redundant write.

## Migration Plan

Four independent PRs in order of value: (1) deletions + corpus folds + AGENTS.md rules, (2) pressure-loop tiering, (3) cache wiring (toolchain default + CI + worktrees), (4) bench extraction. Each is revertible alone; specs archive after all four land.

## Open Questions

- Which stored-callable determinism file becomes the third canary (pick the one with widest artifact surface when implementing — likely `StoredCallableDeterminism` if it covers environments + generics).
- Whether `EditorIntelligence.test.ts` moves to the new IDE package before or after its duplicate cases are pruned (sequencing only; either order works).
