# Speed up the test suite

## Why

`pnpm check` takes ~20 minutes and keeps growing. Six measurement spikes (2026-08-15) located the cost: `packages/compiler` is ~2,580s of the workspace's ~3,100s test CPU, and the dominant waste is redundant full compiler pipelines — 23 per-feature fresh-process determinism files, ~100 per-feature native legs already subsumed by the `DriverNativeAcceptance` differential corpus, and pressure-test rollback loops that run a full native compile per quota ordinal. The native toolchain itself is only ~11% of a native test's cost; vitest overhead is ~4–5%; bun and plain-script conversions were measured and ruled out.

## What Changes

- **Delete redundant compiler tests** (~25% of cases, est. 700–1,200s CPU): keep 3 fresh-process determinism canaries and delete the other 20 `*Determinism.test.ts` files (+ fixtures); fold per-feature "native binary agrees" programs into `test/support/corpus.ts` and drop their standalone `Driver.compile` legs; remove engine-matrix duplicate files, exact diagnostic-message-string assertions (the generated catalog gates wording), and duplicated `EditorIntelligence` cases.
- **Move performance measurements out of the correctness suite**: `SynchronousEffectCost` and `OccurrencePerformance` become an opt-in bench target (or are deleted).
- **Trim pressure-loop native legs** (~200–280s): `LexerPressure` and `StackVmPressure` failure-ordinal sweeps run natively only at boundary ordinals; evaluator and wasm carry intermediate ordinals.
- **Wire the dead native artifact cache**: `NativeToolchain.defaultArtifactCache()` returns a disk cache when `SILK_NATIVE_CACHE_DIR` is set (the env var is currently set by `packages/compiler/vitest.config.ts` and consumed by nothing).
- **Persist caches in CI and across worktrees**: `actions/cache` for `.turbo` (and the native cache dir) in CI; shared `TURBO_CACHE_DIR` for `.claude/worktrees/*`.
- **Add a "Keep tests cheap" section to AGENTS.md** so AI-written tests stop regrowing the waste: cheapest-tier proof obligation, corpus-first native coverage, no per-feature determinism tests, no timing assertions, snapshot reuse.

Not in scope (follow-up candidates): `Analysis` snapshot sharing across engines and stdlib elaboration memoization (~150–250s+, compiler-side work that also benefits the LSP).

## Capabilities

### New Capabilities

_None._

### Modified Capabilities

- `bootstrap-compiler-driver`: fresh-process determinism is consolidated from per-feature gates into designated canary gates; per-feature engine-agreement obligations are discharged by the aggregate differential corpus rather than standalone per-feature native tests.
- `bootstrap-language-pressure-programs`: failure-ordinal sweeps are carried by the evaluator and WebAssembly engines, with native execution required only at boundary ordinals; cross-engine agreement remains required for representative acceptance cases.
- `bootstrap-native-toolchain`: the default artifact cache honors `SILK_NATIVE_CACHE_DIR`, persisting compiled artifacts on disk keyed by content so identical requests skip clang across processes and runs.

## Impact

- `packages/compiler/test/**` (deletions, corpus additions, pressure-loop edits), `packages/compiler/test/support/corpus.ts`
- `packages/compiler/src/NativeToolchain.ts` (`defaultArtifactCache`), `packages/compiler/vitest.config.ts` (comment correction: key includes clang path, not version)
- `.github/workflows/ci.yml` (turbo + native cache persistence), `scripts/turbo.mjs` (worktree-shared `TURBO_CACHE_DIR`)
- `AGENTS.md` (new test-cost rules)
- Expected effect: compiler-suite CPU down ~35–50% from deletions/trims alone; CI additionally gains warm turbo and native caches. No language, compiler, or public API behavior changes other than the opt-in disk cache default.
