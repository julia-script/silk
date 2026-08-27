# Design: CI test sharding spike

## Context

See proposal.md for motivation. Constraints that shape the approach:

- `pnpm check` = `biome check` → `turbo run build` → `turbo run typecheck test` → `test:scripts`.
  All turbo entry points go through `scripts/turbo.mjs`, which derives `--concurrency` from the
  host — shard jobs must keep using it for builds.
- `@silk-lang/compiler#test` is not just vitest: it chains generator checks
  (`unicode:check`, `stdlib:check`, `toolchain:check`), documentation checks/policy/examples,
  `test:parallel` (vitest, all files except `DriverNativeAcceptance.test.ts`), and
  `test:native-acceptance` (that one file, `--maxWorkers=1`). Only `test:parallel` is the
  ~2,580 s payload worth sharding.
- Compiler vitest config sets `maxWorkers: wholeMachineWorkers` and a shared
  `SILK_NATIVE_CACHE_DIR` — both apply per shard job unchanged.
- Turbo `compiler#test` depends on `build`, `documentation#build`, `doctest#build`; a shard job
  satisfies all three with one cache-warm `turbo run build`.

## Goals / Non-Goals

**Goals:** measured wall time, shard balance, flake behavior, and per-file timings for a 4-way
shard of `test:parallel` — enough data to adopt/adjust/abandon.

**Non-Goals:** turbo-cached test results on shard jobs (shards invoke vitest directly, so they
always run; a spike finding, not a spike problem). No package.json or vitest config changes —
the spike lives entirely in `.github/workflows/ci.yml`.

## Decisions

1. **Shard `test:parallel` only, via vitest's native `--shard=N/4`.**
   `pnpm --filter @silk-lang/compiler run test:parallel -- --shard=N/4`.
   The generator/documentation checks are seconds — they stay serial. `DriverNativeAcceptance`
   is already forced to `--maxWorkers=1` and runs once, not per shard.
   *Alternative rejected:* splitting the turbo task — that's the follow-up cache work, not the
   wall-time spike.

2. **Job topology: `validate` keeps everything except compiler vitest; a 4-way `compiler-tests`
   matrix runs the payload.**
   - `validate`: biome, `turbo run build`, `turbo run typecheck`,
     `turbo run test --filter=!@silk-lang/compiler`, then the compiler's non-vitest checks and
     `test:native-acceptance` as explicit steps, then `test:scripts` and `release:candidate`.
     (Typecheck runs unfiltered so `compiler#typecheck` isn't lost.)
   - `compiler-tests` (matrix shard 1–4): checkout/setup/caches/install → `turbo run build` →
     `test:parallel -- --shard=N/4 --reporter=default --reporter=json --outputFile=…` →
     upload timings artifact.

3. **Caching: shards restore the same turbo cache; native cache is restore-only on shards.**
   Turbo cache keys already end in `github.sha`; concurrent saves of the same key are harmless
   warnings (first save wins). The native-executable cache is populated by
   `test:native-acceptance` in `validate`, so shard jobs use `actions/cache/restore` to avoid
   4-way save races.

4. **Measure on the branch's own CI.** Modify `ci.yml` directly on the spike branch — PR runs
   of the modified workflow *are* the measurement. Three re-runs give the flake sample.
   Rollback = revert the ci.yml commit (adopt = merge it).

## Risks / Trade-offs

- [Shard imbalance — vitest shards by file, and the pressure suites are 30 s+ files] → per-shard
  durations in the timing artifacts show it; adjust M or move to duration-aware sharding only if
  the spread is > ~25%.
- [Shard jobs rerun tests even when nothing compiler-related changed (no turbo cache)] → known
  cost of the spike shape; the task-level turbo split is the documented follow-up if it stings.
- [5 jobs × pnpm install/build on cold cache multiplies setup cost] → setup is cache-warm in the
  common case; timings will show the real overhead per shard.
- [Duplicate turbo-cache saves across jobs] → benign warnings; revisit only on adoption.

## Open Questions

- Final shard count (2/4/6) — answered by the spike's own timing data.
- Whether `DriverNativeAcceptance` + checks should later move off `validate`'s critical path —
  answered by where the measured critical path lands.

## Findings (2026-08-26, runs 33016472786 / 33018344159 attempts 1–2 on PR #265)

Caveat: the first run (33014303799) was invalid — `pnpm --filter … run script -- args`
silently drops the forwarded args, so every "shard" ran the full 222-file suite. Fixed by
invoking vitest directly via `pnpm exec` (flags mirrored from `test:parallel`). That invalid
run still measured a useful baseline: the full compiler suite on a dedicated 4-core runner
is ~17 min.

**1. Wall time.** Shard jobs finish in 3–9 min (cache-warm; ~80 s of that is setup + turbo
build). But the gate's wall time is now `validate` at 22–23 min — down from ~30, and the
compiler vitest suite is entirely off the critical path. The ≤12 min target is NOT met by
sharding alone: `validate` still serializes non-compiler package tests (~500–545 s) and
`test:native-acceptance` (~680–734 s, `--maxWorkers=1`).

**2. Turbo caching.** Build steps cache-hit on every shard (~60 s including restore);
concurrent same-key saves are benign warnings as predicted. Shard vitest runs are not
turbo-cached (they always execute) — accepted spike cost, unchanged.

**3. Flakes.** 0 failures across 12 shard jobs + 3 validate runs. No load-amplification
timeouts observed on shard runners. Small sample, but no counterexample.

**4. Time concentration.** Per-file totals (run 33016472786): 3,150 s across 222 files.
`SchedulerFiber.test.ts` alone is 454 s (14%); the top 6 files
(SchedulerFiber, TemporaryDirectoryAcceptance, StackVmPressure, LexerPressure,
BootstrapEvaluation, LocalSharedPressure) are ~1,020 s (~⅓). Shard imbalance is driven by
SchedulerFiber: its shard consistently walls at ~8.5 min while the others run 3–4.5 min.
More shards cannot cut the max below one file's duration.

### Recommendation: adopt, with two adjustments

1. Keep 4 shards. Raising the count is pointless until `SchedulerFiber.test.ts` (454 s) is
   split or duration-aware sharding is introduced — the max shard is single-file-bound.
2. Split `validate` on adoption: `test:native-acceptance` as its own job (~13 min including
   setup/build) and non-compiler tests as another (~10–11 min). Projected gate ≈ 13 min,
   bounded by native acceptance — within sight of the 12 min target; further gains come
   from that suite, not from sharding.

Operational notes for adoption: artifact names carry a `run_attempt` suffix because v4
artifact names are immutable across reruns; a full re-run *deletes* the prior attempt's
artifacts, so download timings before rerunning; empty commits did not trigger
`pull_request` workflow runs on this repo — trigger measurement runs with real changes or
re-runs.
