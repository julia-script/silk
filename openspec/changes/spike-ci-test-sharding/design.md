# Design: CI test sharding spike

## Context

See proposal.md for motivation. Constraints that shape the approach:

- `pnpm check` = `biome check` → `turbo run build` → `turbo run typecheck test` → `test:scripts`.
  All turbo entry points go through `scripts/turbo.mjs`, which derives `--concurrency` from the
  host — shard jobs must keep using it for builds.
- `@silk-effect/compiler#test` is not just vitest: it chains generator checks
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
   `pnpm --filter @silk-effect/compiler run test:parallel -- --shard=N/4`.
   The generator/documentation checks are seconds — they stay serial. `DriverNativeAcceptance`
   is already forced to `--maxWorkers=1` and runs once, not per shard.
   *Alternative rejected:* splitting the turbo task — that's the follow-up cache work, not the
   wall-time spike.

2. **Job topology: `validate` keeps everything except compiler vitest; a 4-way `compiler-tests`
   matrix runs the payload.**
   - `validate`: biome, `turbo run build`, `turbo run typecheck`,
     `turbo run test --filter=!@silk-effect/compiler`, then the compiler's non-vitest checks and
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
