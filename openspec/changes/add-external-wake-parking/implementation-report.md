# Implementation report: add-external-wake-parking

Status: **resumed; fresh hard-gate sequence in progress**

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
| 2.1–2.4 | `ExecutionPark` retains affine registration captures and every ordinary guard shape, including represented callable guards, with exact cleanup; `WakeCell` proves sole generation authority, latch-before-handoff, destruction after latch, dormant notification, no inline eligibility, and duplicate consumption rejection. Source double signal produces `OWN0001`. |
| 3.1–3.5 | `WakeCell` models notification/invocation retains, non-mutating fatal drive admission, DestroyPending, cancelled late-Wake no-op, final-authority release, eligible drop, and generation reuse. `BootstrapEvaluation.ts` transfers final allocation reclaim to an outstanding Wake after Execution cleanup. |
| 3.6 | Wake and every tested containing aggregate/union/array/Shared shape are `LocalExecution`. `SuspensionOwnership` records the owned Shared handle as a local-affine suspended-frame slot. No transfer or atomic operation was added. |
| 4.1 | Deferred-shaped lowering fixture stores/extracts Wake through `Shared.withMut`, signals after access, retains an owned Shared handle across park, and uses an affine-capturing registration. A separate timer-shaped fixture proves the same source boundary. Wake, direct park, parameter-forwarding transitive park, and no-argument transitive park during active `Shared.with`/`withMut` access produce `OWN0016`. |
| 4.2 | `IntrinsicCatalog.test.ts` fixes the allowed inventory to exactly `wake` and `park`, all three targets, safe operations, and no explicit cancel/destroy, Scheduler/timer/payload/allocator surface, or privileged source actor. `ExternalWakeParking.test.ts` exercises semantic analysis, MIR inspection, evaluator availability, and Wasm emission for the sealed operations, while the implementation branches in `Intrinsic.ts`, `LowerBuiltin.ts`, `LowerExpression.ts`, `BootstrapEvaluation.ts`, and `WasmBackend.ts` remain operation-shaped rather than actor-policy-shaped. |

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
5. The access-boundary audit found that synchronous Wake could invoke its external readiness
   endpoint while a local-Shared access loan remained live. Ownership now classifies `ExecutionWake`
   as an external-callback boundary and reports `OWN0016` before lowering.
6. Access-boundary propagation initially followed only helper edges that forwarded callback
   parameter zero. Every synchronous helper still runs under the active loan, so the original
   boundary now propagates through the complete ordinary call graph, including no-argument helpers.
7. An unconstrained generic guard `G` specialized to a represented callable contract, but lowering
   admitted only ordinary type arguments. Instance discovery now retains exact callable result
   identity, `ExecutionPark` realizes that guard with its source contract and environment, and MIR
   verifies its exact callable cleanup.

No compatibility shim, standard-library actor-name recognition, Scheduler policy, hidden allocation,
payload lane, failure channel, cross-thread transfer, or mandatory atomic fact was introduced.

## Verification history

### Focused checks

- `pnpm --filter @silk-effect/compiler exec vitest run test/ExternalWakeParking.test.ts test/WakeCell.test.ts test/IntrinsicCatalog.test.ts test/ExecutionPackage.test.ts test/SuspensionOwnership.test.ts test/Suspendability.test.ts test/SharedStdlib.test.ts` — **PASS**, 7 files / 64 tests.
- `pnpm --filter @silk-effect/compiler exec vitest run test/ExternalWakeParking.test.ts` after evaluator/Wasm never-driven evidence — **PASS**, 1 file / 7 tests.
- `pnpm --filter @silk-effect/compiler typecheck` — **PASS**. One pre-gate run correctly reported stale generated toolchain identity after source edits; `pnpm --filter @silk-effect/compiler toolchain:generate` repaired the generated identity, and the next run passed.
- Targeted Biome check over changed implementation/tests — **PASS** after formatter applied one mechanical wrap.
- `openspec validate add-external-wake-parking --strict --json --no-interactive` — **PASS**, 1/1.
- Post-conformance focused sequence: compiler typecheck plus
  `ExternalWakeParking.test.ts`, `SharedStdlib.test.ts`, `OwnedAllocationAcceptance.test.ts`,
  `WakeCell.test.ts`, and `Mir.test.ts` — **PASS**, 5 files / 58 tests.

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

### Post-conformance hard-gate rerun

The contract permits one hard-gate rerun after the consolidated conformance fix pass. Its exact
result was:

- `pnpm typecheck` — **PASS**.
- `pnpm exec biome check .` — **PASS**.
- `pnpm test` — **FAIL** in the compiler documentation check before compiler tests:
  `diagnostics.md is stale. Run pnpm --filter @silk-effect/compiler documentation:generate`.
- `pnpm check` — **NOT RUN** because the chained gate stopped at `pnpm test`.
- `pnpm release:candidate` — **NOT RUN** because the chained gate stopped at `pnpm test`.

The stale generated diagnostic documentation follows the new `OWN0016` external-callback wording;
it was not regenerated after this bounded rerun failed. No second rerun is permitted, so the change
is parked with the focused 58-test repair evidence green and the exact remaining mechanical blocker
recorded.

### Fresh bounded resume

The fresh resume began from merge commit `3fa2d28`, which contains the parked implementation and
the single consolidated conformance fix pass through `91a622b`. It does not repeat that conformance
pass because the resume has introduced no semantic implementation change.

- The first documentation-generation attempt could not load
  `packages/documentation/dist/Project.js` in the new worktree. Relocalization showed that the
  documentation package in turn consumes built compiler subpaths and the compiler consumes built
  LLVM/Wasm subpaths. One root topological `pnpm build` supplied the missing workspace outputs; this
  is resume root-cause repair 1/3 and did not change tracked source.
- `pnpm --filter @silk-effect/compiler documentation:generate` — **PASS** after the prerequisite
  build; regenerated `packages/language/docs/diagnostics.md` with the `OWN0016` external-readiness-
  callback form.
- `pnpm --filter @silk-effect/compiler documentation:check` — **PASS**.
- Focused post-conformance regression sequence over `ExternalWakeParking.test.ts`,
  `SharedStdlib.test.ts`, `OwnedAllocationAcceptance.test.ts`, `WakeCell.test.ts`, and `Mir.test.ts`
  — **PASS**, 5 files / 58 tests.
- `openspec validate add-external-wake-parking --strict --json --no-interactive` — **PASS**, 1/1.
- Fresh full hard gates — **PENDING**.

## Conformance findings

The required single pass used separate language/SLP behavior, OpenSpec/task-evidence, and
architecture/minimal-privilege lenses. Findings and verified dispositions:

| Lens claim | Severity | Disposition |
| --- | --- | --- |
| Wake may consume under active `Shared.withMut` access. | High | **Verified and fixed.** `Ownership.ts` now classifies synchronous `ExecutionWake` as an external callback while the access boundary is active; the exact fixture produces `OWN0016`. |
| Transitive park through a helper that does not forward the borrowed parameter bypasses the access boundary. | High | **Verified and fixed.** Boundary spans now traverse every ordinary synchronous helper edge; direct, forwarding, and no-argument helpers all produce `OWN0016`. |
| A callable-valued registration guard produces no `ExecutionPark` operation or retained cleanup. | High | **Verified and fixed.** `ExecutableOrigin.ts`/`Instances.ts` retain exact callable result identity; `LowerExpression.ts` realizes the guard and `MirVerification.ts` verifies its environment cleanup. The regression has one valid `ExecutionPark` with `CallableCleanup`. |
| Layer 3 must already execute park/wake in each engine because evaluator traps and Wasm emits `unreachable`. | High | **Rejected as the explicit DAG boundary.** `add-independent-execution-engine-parity/proposal.md` identifies itself as realization slice 4 and says it realizes the preceding target-neutral contracts across supported engines; its tasks 1.1–4.3 own complete transition MIR, evaluator, native/Wasm, and differential execution. Layer 3 keeps those not-yet-supported paths fail-closed. |
| The report improperly defers Layer 3 requirements to a duplicative Layer 4. | High | **Rejected.** Layer 4's proposal depends on this change, describes this layer's output as established package/drive/park/Wake/cleanup contracts, and explicitly owns their engine realization. This report records rather than invents that handed-off boundary. |
| Shared survival and timer fixtures must execute park/wake/resume in this layer. | High | **Rejected as Layer 4 differential scope.** This layer verifies source admission, exact suspension ownership/local-Shared obligations, access boundaries, and never-driven cleanup/emission. Layer 4 tasks 2.1–4.3 own runtime park/resume, same-thread reactor delivery, and differential cleanup cases. |
| External-park reachability reads the sealed intrinsic actor/name metadata rather than the closed `ExecutionPark` HIR operation identity. | Medium | **Verified, recorded, no conformance fix.** The predicate predates this slice and consumes sealed Intrinsic metadata rather than an ordinary library declaration, so it does not add standard-library actor privilege. The complete Layer 4 MIR authority pass should canonicalize this identity check; the bounded fix pass is restricted to verified Critical/High findings. |

One consolidated fix pass addressed the three verified High findings. No Critical finding remained.
