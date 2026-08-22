# SLP-0002 implementation plan and outcome

- SLP: `0002-allocation-backed-local-shared-ownership`
- Accepted-direction revision: 6
- Accepted-direction digest: `c97959718e551d9d4c4273e6503a18630696c6ac969087192bc3e5133c4ca069`
- Integration branch: `julia/slp-0002-implement`
- Audited handoff baseline: `987ce26`
- Run date: 2026-08-22
- Outcome: Parked at layer 2 hard gates

## Dependency plan

| Layer | OpenSpec change | Dependency | Outcome | Commit or range |
| --- | --- | --- | --- | --- |
| 1 | `establish-local-shared-ownership` | none | Done and merged | `987ce26..07a5977`; merge `3d5d745` |
| 2 | `add-local-shared-control-block-allocation` | layer 1 | Parked/gates; not merged | `3d5d745..fd6b981` |
| 3 | `add-local-shared-lifecycle-operations` | layers 1-2 | Parked/dependency; not started | none |
| 4 | `add-local-shared-standard-library` | layers 2-3 | Parked/dependency; not started | none |
| 5 | `add-local-shared-engine-parity` | layers 1-4 | Parked/dependency; not started | none |
| 6 | `prove-local-shared-slp1-sufficiency` | layers 4-5 | Parked/dependency; not started | none |

The DAG has width one. No dependent change was started after layer 2 parked.

## Handoff audit

All six OpenSpec changes passed strict validation and have `Result: Ready` audit records. Audit repairs were committed at `987ce26` before implementation worktrees were created.

## Layer 1: ownership semantics

OpenSpec state: 14/14 tasks complete. Implementation commit `07a5977` was merged as `3d5d745` after its worktree gates and conformance pass completed.

Pre-conformance gate attempts:

1. Typecheck and Biome passed. `pnpm test` found unbounded recursion while expanding a polymorphically recursive nominal in `ExecutionAffinity`; the traversal was re-localized and guarded by declaration identity.
2. Typecheck, Biome, tests, and `pnpm check` passed. `pnpm release:candidate` found that the expected export manifest omitted the new public actors; the release-candidate fixture was updated.
3. All required gates passed: typecheck 24/24, full tests 28/28, `pnpm check` 42/42 plus 16/16 script tests, and release-candidate build 14/14 plus validation 9/9.

The single three-lens conformance pass verified five High findings. The one permitted High-only fix pass repaired:

- sealed `SharedCore` provenance and same-spelling non-forgeability;
- the absence of ordinary construction and target-layout privilege;
- retained borrow-root affinity in executable captures;
- concrete callable/Effect environment facts across moves and suspension;
- logical zero-lane frame obligations; and
- ownership fabrication through `Slot<SharedCore>` and raw buffers.

The mandatory post-conformance rerun passed in full. Eight Medium findings were recorded without changes under the High-only fix rule: locale-dependent parameter ordering, unavailable-capture causal evidence, several focused-matrix/assertion gaps, a missing separate semantic golden, and the absence of a persistent gate-results artifact before this record. No Critical or unresolved High finding remained.

The root integration barrier then passed `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, and `pnpm release:candidate` (9/9 validation).

## Layer 2: control-block allocation

The parked work is preserved on branch `julia/slp0002-add-local-shared-control-block-allocation` at `fd6b981`. OpenSpec tasks 1.1 through 3.2 are checked; gate task 3.3 remains unchecked. Focused validation passed 4 files and 41 tests.

The implementation contains the sealed `sharedLayout<T>` contract, unsafe consuming `sharedFromAllocation<T>`, target-aware control-block planning and provenance diagnostics, consuming MIR and verification, evaluator/LLVM/direct-Wasm initialization parity, private reclaim lanes, inspector support, and focused negative, exhaustion, same-spelling, and native-corpus coverage.

The three bounded hard-gate attempts ended as follows:

1. `pnpm typecheck` failed because the inspector was not exhaustive for the new shared value and operations. The implementation was re-localized through the graph and repaired.
2. Typecheck passed; `pnpm exec biome check .` failed for an unused Wasm binding and formatting/import drift. The mechanical issues were repaired.
3. Typecheck and Biome passed; `pnpm test` stopped because `packages/compiler/diagnostics.md` was stale and prescribed `pnpm --filter @silk-effect/compiler documentation:generate`.

The third distinct failure exhausted the change's gate-fix budget. The change was not merged, `pnpm check` and `pnpm release:candidate` were not run, and the conformance lenses were not started because their gate prerequisite was not satisfied. There is therefore no layer-2 conformance findings ledger.

## Resume point

A later explicit SLP-5 implementation run can resume the parked layer-2 branch at `fd6b981`. It must regenerate the diagnostic documentation, complete task 3.3 only after the full ordered gates pass, perform the single required three-lens conformance pass, and cross the root integration barrier before layers 3-6 are unparked.

SLP-0002 is not ready for `slp-6-audit-implementation` or archive while layers 2-6 remain parked.
