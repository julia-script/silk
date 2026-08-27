# Split CI validate's critical path

## Why

The sharding spike (archived: `2026-08-26-spike-ci-test-sharding`) took the compiler suite
off the CI critical path, but the gate is still ~17–23 min because `validate` serializes two
big steps: non-compiler package tests (~500 s) and `test:native-acceptance` (~700 s,
`--maxWorkers=1`). The spike's findings project a ~13 min gate once those run as their own
jobs. Separately, shard balance is capped by `SchedulerFiber.test.ts` — one 454 s file (14%
of all compiler test CPU) — so shard 1 walls at ~8.5 min while the others finish in 3–4.5.

## What Changes

- Split `.github/workflows/ci.yml`'s `validate` job:
  - `test:native-acceptance` (plus whatever native-cache save it owns) → its own job;
  - `turbo run test --filter='!@silklang/compiler'` → its own job;
  - `validate` keeps biome, build, typecheck, the compiler's generator/documentation
    checks, `test:scripts`, and `release:candidate`.
- Re-measure the gate (expect ~13 min, bounded by native acceptance).
- Split `SchedulerFiber.test.ts` into smaller files (or adopt duration-aware sharding) so
  the max shard drops toward the ~3–4.5 min pack.
- Revisit the native-executable cache save/restore topology once native acceptance moves
  (today: `validate` saves, shards restore-only).

## Non-goals

- Changing shard count (4 stays right until the SchedulerFiber cap moves).
- Task-level turbo cache splitting for compiler tests (separate exploration).
- Any change to what is tested.

## Capabilities

None — CI tooling and test-file reorganization only (`skip_specs: true`).

## Impact

- `.github/workflows/ci.yml`; `packages/compiler/test/SchedulerFiber.test.ts` (split only,
  no behavioral change to assertions).
- Success criterion: PR gate ≤ 13 min over 3 consecutive green runs; max shard ≤ 6 min
  after the SchedulerFiber split.
