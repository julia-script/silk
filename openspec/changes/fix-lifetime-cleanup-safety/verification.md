# Critical lifetime cleanup fixes

Validated merged source commit: `5307ad8e` on `julia/lifetimes-critical-fixes`.
Implementation base: `3ca10d4d09f666ff5161f14554f905986bfb486b`.

## Merge validation — 2026-09-05

Merged main at `8b4024a8` into the branch. The sole conflict was `ToolchainIntegrity.generated.ts`; the normal generator recomputed its digest from the combined source tree. Both branches' source changes were preserved without a manual semantic resolution.

Every local gate passed on the merged tree: `pnpm typecheck`, `pnpm format:check`, `pnpm lint`, `pnpm test`, `pnpm check`, and `pnpm release:candidate`. Strict validation of this OpenSpec change also passed.

- Fresh compiler suite: 2,276 tests across 211 files, including all 83 ownership tests.
- Fresh native acceptance: 317 tests, all passing.
- Standard-library doctests: 60 passed.
- Workspace tests: 22 successful tasks, 11 cached; 26m15s overall.
- `pnpm check`: passed, reusing successful build/typecheck/test tasks and running formatting, lint, and all 17 script checks.
- Release candidate: 10 tests passed in 35 seconds, with cached prerequisite builds.
- GitHub CI also passed every check on merge commit `5307ad8e`.

The conflict is resolved. No implementation edits followed this validation.

## Earlier successful retry — 2026-09-05

After disk space was freed, every required local gate passed:

- `openspec validate fix-lifetime-cleanup-safety --strict`.
- `pnpm typecheck` (18 cached tasks).
- `pnpm format:check` and `pnpm lint`.
- `pnpm test` (22 successful tasks, 14 cached): the fresh compiler suite passed all 2,275 tests across 211 files, including the 83 ownership tests; all 316 native acceptance tests passed; all 60 standard-library doctests passed. The complete workspace test command finished in 23m51s.
- `pnpm check`: passed, reusing build/typecheck/test results and freshly running formatting, lint, and script checks.
- `pnpm release:candidate`: passed all 10 tests in 27 seconds; prerequisite build tasks were cache hits.

No implementation code changed during this retry. Task 3.1 is complete. This earlier run validated implementation commit `1d230105`; the merge validation above covers the combined source tree.

## Regression evidence

`pnpm --filter @silklang/compiler exec vitest run test/Ownership.test.ts --maxWorkers=1`

- Before compiler edits: 4 failures, 79 passes. Both invalid installed-borrow witnesses were accepted, and both match and statement-pattern selected entries lacked flag resets.
- After ISSUE-004 alone: 2 failures, 81 passes. Installed lifetime rejection and the earlier-referent control passed; pattern regressions remained red.
- After both fixes: 83 passes. Both selected-execution entries reset the same flag cleared by conditional partial cleanup. MIR verification passed.
- Final rejection assertions pin the installed borrow's diagnostic code and origin span. The full successful retry includes these final assertions.

## Earlier attempts

The initial workspace test run timed out in docgen's fenced-example compilation after 180 seconds. The cause was not established; the same suite passed on retry. Two earlier `pnpm check` attempts failed with disk-space errors, first in the docs build and then during Turbo log handling. These failures were resolved by the successful retry above.

An intermediate compiler run had two failures in new diagnostic-origin span assertions that omitted leading whitespace. Those assertions were corrected before the successful full retry; compiler implementation code did not change afterward.

Raw logs for the initial runs and the successful retry are retained under `.scratch/lifetimes-critical-fixes/` in this worktree. This directory is gitignored.
