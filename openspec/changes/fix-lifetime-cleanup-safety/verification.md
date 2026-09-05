# Critical lifetime cleanup fixes

Base: `3ca10d4d09f666ff5161f14554f905986bfb486b`.
Branch: `julia/lifetimes-critical-fixes`.

## Regression evidence

`pnpm --filter @silklang/compiler exec vitest run test/Ownership.test.ts --maxWorkers=1`

- Before compiler edits: 4 failures, 79 passes. Both invalid installed-borrow witnesses were accepted, and both match and statement-pattern selected entries lacked flag resets.
- After ISSUE-004 alone: 2 failures, 81 passes. Installed lifetime rejection and the earlier-referent control passed; pattern regressions remained red.
- After both fixes: 83 passes. Both selected-execution entries reset the same flag cleared by conditional partial cleanup. MIR verification passed.
- The rejection assertions also pin the installed borrow's diagnostic code and origin span.

## Repository gates

- `openspec validate fix-lifetime-cleanup-safety --strict`: passed.
- `pnpm typecheck`: passed.
- `pnpm format:check`: passed.
- `pnpm lint`: passed.
- Initial `pnpm test`: docgen's standard-library fenced-example compilation timed out at 180000ms. The pinned review previously recorded this test passing in approximately 93 seconds; the cause of this run's timeout is not established.
- Initial `pnpm check`: docs Next.js build failed with `ENOSPC` while writing `_clientMiddlewareManifest.js`. This is an environment failure, not a compiler assertion. The worktree's generated `.next` output was cleaned afterward.

- Compiler package run: generated-data, documentation policy, and all 60 stdlib doctests passed. The 211-file non-native suite reported 2273 passes and two failures in the new diagnostic-origin span assertions, which incorrectly omitted leading whitespace. After correcting those assertions, the full 83-test ownership file passed again. No compiler implementation edits followed that broader run.
- Second `pnpm check`: docs build passed, but Turbo aborted during test scheduling with `StorageFull` / `Cannot write logs: No space left on device (os error 28)`.

Full integration validation remains incomplete. Native acceptance was not reached. The initial docgen timeout is not established as pre-existing; the repeated storage errors are environmental. Raw logs are retained under `.scratch/lifetimes-critical-fixes/` in this worktree.

`pnpm release:candidate`: passed, 10/10 tests (34.72 seconds); prerequisite build tasks were cache hits.
