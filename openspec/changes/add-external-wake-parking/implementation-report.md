# Implementation report: add-external-wake-parking

Status: **implementation and hard gates complete; conformance pending**

## Scope and layer boundary

This change implements the ordinary-source contract and target-neutral ownership protocol for
external Wake parking. `Intrinsic.Wake`, `Intrinsic.wake`, `Intrinsic.park`, external-park
reachability, package wake storage, cleanup authority, LocalExecution affinity, Shared access-loan
admission, MIR lowering, deterministic inspection, and the canonical wake-cell transition system
are complete here.

Actual non-LIFO continuation execution is intentionally the next DAG layer, not a deferred task of
this change. The boundary is explicit in
`openspec/changes/add-independent-execution-engine-parity/tasks.md`: tasks 1.1–1.3 require the
complete transition MIR/inspection consumer, tasks 2.1–2.3 require the evaluator oracle, and tasks
3.1–4.3 require native/Wasm realization and differential runtime scenarios. Its
`specs/bootstrap-independent-execution-engine-parity/spec.md` likewise owns supported-engine
agreement for park, wake, notification, cancellation, DestroyPending, and traps. Until that layer,
the evaluator and Wasm operation branches remain fail-closed. This layer does verify that a
park-capable never-driven package constructs, cleans, and emits/runs on evaluator and Wasm without
executing the stored body.

## Task evidence

| Tasks | Evidence |
| --- | --- |
| 1.1 | `Type.ts`, `Intrinsic.ts`, `NameResolution.ts`, `Layout.ts`, `LayoutVerify.ts`, `ExecutionAffinity.ts`, generated stdlib/toolchain; `ExternalWakeParking.test.ts` checks sealed identity, fixed non-copy layout, aggregate affinity, callable cleanup, target inventory, and ordinary lowering. |
| 1.2 | `Instances.representedEffectSuspensionOf` preserves owner-scoped represented body identity; `Layout.ts` and `ExecutionPackage.ts` select WakeControl only for `ExternalPark`; package ownership encodes `StableGenerationCell` and `IndivisibleUntilFinalAuthority`. Existing direct/nested package regression tests remain green. |
| 1.3 | `WakeCell.ts`, `Mir.ts`, `MirEncoding.ts`, `MirLinearization.ts`, `MirVerification.ts`, `ProvisionalMir.ts`, `SuspensionOwnership.ts`, and `OwnershipEncoding.ts`; malformed authority and deterministic encoding tests are in `WakeCell.test.ts`. |
| 2.1–2.4 | `ExecutionPark` retains affine registration captures and guard cleanup; `WakeCell` proves sole generation authority, latch-before-handoff, destruction after latch, dormant notification, no inline eligibility, and duplicate consumption rejection. Source double signal produces `OWN0001`. |
| 3.1–3.5 | `WakeCell` models notification/invocation retains, non-mutating fatal drive admission, DestroyPending, cancelled late-Wake no-op, final-authority release, eligible drop, and generation reuse. `BootstrapEvaluation.ts` transfers final allocation reclaim to an outstanding Wake after Execution cleanup. |
| 3.6 | Wake and every tested containing aggregate/union/array/Shared shape are `LocalExecution`. `SuspensionOwnership` records the owned Shared handle as a local-affine suspended-frame slot. No transfer or atomic operation was added. |
| 4.1 | Deferred-shaped lowering fixture stores/extracts Wake through `Shared.withMut`, signals after access, retains an owned Shared handle across park, and uses an affine-capturing registration. A separate timer-shaped fixture proves the same source boundary. Direct and helper-transitive park during active `Shared.with`/`withMut` access produce `OWN0016`. |
| 4.2 | `IntrinsicCatalog.test.ts` fixes the allowed inventory to exactly `wake` and `park`, all three targets, safe operations, no explicit cancel/destroy, Scheduler/timer/payload/allocator surface, or privileged source actor. |

## Implementation localization and fixes

1. `ExternalPark` initially disappeared when a represented body crossed owner-scoped effect
   identity. `Instances.representedEffectSuspensionOf` now resolves that identity, so layout and
   package planning agree.
2. An affine-capturing registration exposed an overly shallow `ExecutionPark` cleanup verifier.
   Verification now checks exact callable-environment take fields, identities, ordinals, and cleanup
   plans.
3. A transitive `Shared.withMut` fixture exposed that access-loan boundary spans stopped at a helper
   call. `Ownership.localSharedAccessBoundaryPlan` now propagates the original boundary through
   forwarding calls; direct and transitive cases both reject before suspension.
4. Execution cleanup initially released a readiness package unconditionally. It now applies
   `WakeCell.destroyExecution` and leaves the indivisible allocation retained when a Wake remains;
   Wake cleanup owns the final release.

No compatibility shim, actor-name recognition, Scheduler policy, hidden allocation, payload lane,
failure channel, cross-thread transfer, or mandatory atomic fact was introduced.

## Verification history

### Focused checks

- `pnpm --filter @silk-effect/compiler exec vitest run test/ExternalWakeParking.test.ts test/WakeCell.test.ts test/IntrinsicCatalog.test.ts test/ExecutionPackage.test.ts test/SuspensionOwnership.test.ts test/Suspendability.test.ts test/SharedStdlib.test.ts` — **PASS**, 7 files / 64 tests.
- `pnpm --filter @silk-effect/compiler exec vitest run test/ExternalWakeParking.test.ts` after evaluator/Wasm never-driven evidence — **PASS**, 1 file / 7 tests.
- `pnpm --filter @silk-effect/compiler typecheck` — **PASS**. One pre-gate run correctly reported stale generated toolchain identity after source edits; `pnpm --filter @silk-effect/compiler toolchain:generate` repaired the generated identity, and the next run passed.
- Targeted Biome check over changed implementation/tests — **PASS** after formatter applied one mechanical wrap.
- `openspec validate add-external-wake-parking --strict --json --no-interactive` — **PASS**, 1/1.

### Required hard gates

The bounded gate loop used three distinct root-cause repairs:

1. The first `pnpm typecheck` found non-exhaustive inspector projections for the new Wake value,
   cleanup, and MIR operation variants. `ProjectBackend.ts` gained explicit projections.
2. The restarted sequence passed typecheck and Biome, then `pnpm test` found that the new
   `Execution.park` documentation did not satisfy the standard-library SummaryShape policy. The
   comment was repaired under the `silk-doc-comments` policy; policy, 54 doctests, generated docs,
   and checked docs then passed.
3. The next sequence passed typecheck, Biome, the full test suite, and `pnpm check`, then
   `pnpm release:candidate` correctly rejected an unintended public compiler-root `WakeCell`
   export. The target-neutral protocol remains compiler-private and the root export was removed.

After the third and final permitted repair, the complete required sequence passed without another
change:

- `pnpm typecheck` — **PASS**.
- `pnpm exec biome check .` — **PASS**, 986 files checked with no fixes.
- `pnpm test` — **PASS**, 28/28 workspace tasks; compiler 219 files / 2,117 tests plus the native
  differential corpus, and every remaining package suite passed.
- `pnpm check` — **PASS**, 42/42 Turbo tasks plus 16/16 repository script tests.
- `pnpm release:candidate` — **PASS**, 14/14 builds and 9/9 release-candidate validations.

## Conformance findings

Pending the required single three-lens pass after the hard gates.
