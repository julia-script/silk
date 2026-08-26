# Spike: shard CI tests across matrix runners

## Why

CI wall time is ~30 min and grows with every test added. The bottleneck is not cache
granularity — it is serialization: one 4-core `ubuntu-latest` job runs build, typecheck,
and every package's tests back to back, while the compiler suite alone carries ~2,580 s of
test CPU. The repo is public, so additional standard runners cost nothing. Sharding the
test phase across a matrix should cut iteration time to ~8–10 min without restructuring
any package, and reduces the load-amplification flakes documented in `vitest.shared.ts`
(each shard gets its own 4 cores instead of sharing a saturated runner).

This is a spike: the goal is a measured answer, not a polished pipeline.

## Questions the spike must answer

1. What wall time does a 4-way shard of the compiler suite actually achieve (vs. the
   ~10 min back-of-envelope)?
2. Does per-shard turbo caching still work, or do shards fight over the cache key?
   (`vitest --shard` runs inside one turbo `test` task — the task cache is all-or-nothing
   per shard, so the shard index must be part of what turbo hashes or caching must be
   accepted as build-only on shard jobs.)
3. Do the pressure/determinism suites stop flaking when they no longer share a runner
   with the rest of `pnpm check`?
4. Where does the 2,580 s actually concentrate? (Capture per-file timings from the shard
   runs — this data decides whether a follow-up "slow lane" is worth more than caching.)

## What Changes

- Add a spike branch CI workflow (or modify `ci.yml` behind a matrix) that:
  - keeps one job for `biome check`, build, typecheck, and non-compiler package tests;
  - fans the compiler test suite out over `vitest run --shard=N/M` matrix jobs
    (start with M=4; measure M=2 and M=6 if 4 looks off);
  - preserves the turbo build cache and the native toolchain cache per job.
- Collect timings: total wall clock per configuration, per-shard duration spread
  (shard balance), and per-test-file durations (`--reporter=json`).
- Write findings back into this change (design.md or a findings section) with a
  recommendation: adopt / adjust shard count / abandon.

## Non-goals

- Splitting `packages/compiler` into multiple packages (explored separately; import
  clustering — 182/228 test files import `Analysis` — bounds the cache win).
- Task-level turbo cache splitting (`test:frontend`/`test:backend` inputs scoping) —
  a candidate follow-up, informed by this spike's timing data.
- Any change to test content, timeouts, or `vitest.shared.ts` policy.

## Capabilities

None — CI tooling only, no spec-level behavior changes (`skip_specs: true`).

## Impact

- `.github/workflows/ci.yml` (spike variant; revert or adopt at the end).
- No source or package changes.
- Success criterion: PR-iteration wall time measured at ≤ 12 min with no new flakes
  across 3 consecutive runs, or a documented reason sharding does not deliver it.
