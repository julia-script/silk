# JUL-117 verification

## Capability evidence

- `RuntimeSliceOwnership.test.ts` covers stored exclusive ancestry, copied shared descendants,
  conservative dependent Drop, stable replacement, whole-child extraction, partial cleanup after
  incoming failure, and restricted MIR reference-descriptor use.
- `OwnedAllocation.test.ts` covers Slot storage lifetimes, extraction of external payloads,
  non-escaping allocation access, and invariant payload rejection.
- `VectorAcceptance.test.ts` covers shared and exclusive elements, ordinary replacement and
  extraction, and backing-source invalidation while extracted values or destructors remain live.
- The shared native case `vector-dependent-elements-cleanup-and-extraction` covers growth,
  insertion, replacement, remove/pop, complete Drop-child transfer, allocation failure, and
  exactly-once dependent destruction. Existing interruption and partial-suspension restrictions
  remain in force; fatal traps retain their no-unwind contract.
- `Instances.test.ts` and `ProjectAnalysis.test.ts` cover lifetime-erased instance/layout reuse
  and declaration-surface invalidation. New Drop declarations change the global conformance
  catalog; hook-body edits preserve unrelated semantic reuse.
- `StructuralUnionRuntime.test.ts` verifies that union conversions retain instantiated executable
  identity separately from lifetime-erased cached layouts and reject corrupted source evidence.
- Workload observations and reproduction commands are in
  `packages/compiler/benchmarks/lifetimes-dependent.md`. All 18 source samples matched their
  acceptance expectations; separate counters report region, loan, cleanup, comparison, and
  residual work.

## Repository gates

The final ordered run on 2026-09-05 passed every required gate:

| Command                                                               | Result                                                                                 |
| --------------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| `pnpm typecheck`                                                      | 18 tasks passed                                                                        |
| `pnpm format:check`                                                   | Passed                                                                                 |
| `pnpm lint`                                                           | Passed                                                                                 |
| `pnpm test`                                                           | 22 tasks passed; 2,258 compiler tests and all 311 native acceptance tests passed       |
| `pnpm check`                                                          | Build, lint, 33 cached typecheck/test tasks, and all 17 repository-script tests passed |
| `pnpm release:candidate`                                              | Build and all 10 package validation tests passed                                       |
| `openspec validate add-exclusive-storage-and-dependent-drop --strict` | Passed                                                                                 |

The full native acceptance run took 861.01 seconds and used no corpus selector. The aggregate
`pnpm check` reused the successful final test run through Turbo. The release-candidate tests ran
against the built package contents.

Earlier full runs exposed two implementation regressions: duplicate raw-buffer Slot loan endings
and loss of executable identity in union verification. Both were fixed and their focused witnesses
pass. Another run timed out in the existing Driver phase-report and host C-layout tests under
parallel load; both passed when rerun together with one worker. No test timeouts were increased.

## Main integration

Fetched `origin/main` at `665b5684` on 2026-09-05 and checked the implementation commit `90fa49ad`
with `git merge-tree --write-tree --name-only`. This read-only check found three conflicts:

- `DeclarationCollection.ts`: combine main's inherited lifetime `environment` with intrinsic
  nominal parameter discovery.
- `DeclarationCompletion.ts`: retain main's shared resolver and inherent-head finalization;
  return intrinsic nominal parameters when the nominal has no ordinary declaration.
- `ToolchainIntegrity.generated.ts`: regenerate after combining sources.

The user subsequently requested the merge. All three conflicts are resolved, preserving the
inherent-impl lifetime environment and intrinsic nominal parameters, with a regenerated toolchain
fingerprint. The first combined-tree tests caught intrinsic Slot lifetime inference being applied
to local declarations named Slot. Inherent owner identity now takes precedence during header
elision as it already did during owner completion; genuine storage types retain their intrinsic
lifetime parameters. The override is limited to impl headers: ordinary annotations such as an
imported `Slot<i32>` retain intrinsic lifetime elision. All 117 focused tests pass across
InherentImpl, DeclarationIndex, OwnedAllocation, RuntimeSliceOwnership, VectorAcceptance, and
StdlibNamespaceAcceptance. The full merged-tree gates are in progress before the merge commit is
finalized.
