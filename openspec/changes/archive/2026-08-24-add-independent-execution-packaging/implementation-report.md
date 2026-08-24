# Implementation report

## Result

Done. All fifteen OpenSpec tasks are complete and the required focused, repository, and release
gates pass after the bounded resume and the single three-lens conformance pass.

The slice adds exact target execution-package planning and provenance, consuming construction from
owned allocation, take-once drive callbacks, evaluator logical-root ownership, nested transfer
handling, exact cleanup and resource accounting, direct WebAssembly execution, and the ordinary
source `silk/execution` wrapper. It preserves the callback and ownership seam needed by the next
wake/parking slice without implementing that slice's Wake producer, notification ordering, or
Dormant-to-Eligible runtime policy.

## Resumed finite-join diagnosis

The bounded resume first established this deterministic red loop:

```text
pnpm --filter @silk-effect/compiler exec vitest run \
  test/EffectJoin.test.ts \
  -t 'preserves the selected alternative capture shape' \
  --reporter=dot

Expected Completed, received Blocked.
```

The focused test was strengthened to verify MIR before evaluation. The exact failure was an
`InvalidAggregate`: a finite composite represented Effect had no concrete callable environment.
Tracing `Layout.layoutType` through `layoutDirectRepresented`, `LayoutVerify.verifyEntry`,
`MirVerification.verify`, and `BootstrapEvaluation.evaluate` established that Layer 2 correctly
materialized standalone represented executable storage, but the verifier still recognized only
single exact represented callables and Effects. Its fallthrough invalidated otherwise canonical
finite composite Effect storage, and evaluation therefore reported `Blocked` before execution.

Ranked hypotheses recorded before testing were:

1. the represented-layout generalization exposed a missing composite case in `LayoutVerify`;
2. represented-type resolution collapsed the finite composite to one wrapper;
3. generalized builtin type collection enrolled unrelated represented Effects; and
4. layout-key aliasing poisoned the target catalog.

The first hypothesis was confirmed. `Layout.plan` now retains every exact represented alternative
needed to verify a finite composite, and `LayoutVerify` checks the canonical tag/payload size,
alignment, copy, and zero-field storage facts derived from those alternatives.

## Complete attempt history

The original bounded run used its three implementation fixes and then parked on the newly exposed
finite-join regression:

1. Added exhaustive inspector rendering for the new cleanup, MIR-operation, and bootstrap-value
   variants after downstream typecheck failed.
2. Repaired import/type-only hygiene and removed the stale represented-layout parameter after
   Biome failed.
3. Added the required standard-library documentation section and regenerated documentation
   artifacts after the documentation policy failed.
4. Parked without a fourth fix when the full suite exposed ten finite-join failures across
   `BootstrapEvaluation.test.ts` and `EffectJoin.test.ts`.

The fresh resume used exactly three distinct root-cause fixes:

1. Restoring the historical containing-union-only represented-layout gate made all eleven
   `EffectJoin` tests pass but broke six execution-package tests by removing required standalone
   result, runner, and callable shapes. This disproved that design and was reverted.
2. Adding canonical finite-composite verification plus exact-alternative plan entries preserved
   independent package planning and made the combined focused suite pass. TypeScript narrowing and
   formatter output encountered while completing this fix were mechanical continuations, not new
   root causes.
3. `pnpm release:candidate` then found the new public `ExecutionPackage` actor missing from its two
   explicit compiler API allowlists. Both release assertions were updated; the release suite
   passed 9/9.

The later conformance fix pass is recorded separately below; it was the one permitted consolidated
pass for verified Critical/High lens findings.

## Final focused and hard-gate evidence

After the conformance fix, the focused command passed six files and 59 tests:

```text
pnpm --filter @silk-effect/compiler exec vitest run \
  test/ExecutionPackage.test.ts \
  test/EffectSuspensionWasm.test.ts \
  test/IntrinsicCatalog.test.ts \
  test/Layout.test.ts \
  test/SuspensionOwnership.test.ts \
  test/EffectJoin.test.ts \
  --reporter=dot

Test Files  6 passed (6)
Tests       59 passed (59)
```

The required repository gates then produced:

| Gate | Final evidence |
| --- | --- |
| `pnpm typecheck` | 24/24 Turbo tasks passed. |
| `pnpm exec biome check .` | 983 files checked, no fixes required. The preceding invocation requested only the formatter's one-line layout for the new assertion; `--write` applied it before this clean result. |
| `pnpm test` | 28/28 Turbo tasks passed in 11m26s; compiler 217/217 files and 2101/2101 tests; native differential corpus 1/1; all downstream suites passed. |
| `pnpm check` | Biome clean; 42/42 cached/build/typecheck/test tasks passed; 16/16 repository script tests passed. |
| `pnpm release:candidate` | Build 14/14 tasks; release validation 1/1 file and 9/9 tests passed. |

The focused cases cover direct, nested, and external-capable package planning; provenance mismatch
and overflow; Initial and Eligible admission; fatal Dormant and Notifying admission; independent
roots; never-driven and completion cleanup; allocation refusal; nested evaluator and WebAssembly
transfer; fatal stack exhaustion; non-LIFO roots; typed-failure reification; finite represented
Effect joins; and observable exact body/endpoint Drop execution in WebAssembly package exits.

## Three-lens conformance ledger

Exactly one language/SLP, OpenSpec/tasks, and architecture/minimal-privilege pass ran with separate
read-only agents, at most two concurrently. Findings were verified locally and one consolidated
fix pass addressed the verified High defect.

| Claim | Severity | Disposition and evidence |
| --- | --- | --- |
| WebAssembly completion released the package without fixed endpoint/callback cleanup; never-driven release also used opaque represented cleanup rather than the exact realized body/callback plans. | High | Verified and fixed. MIR initialization's exact cleanup metadata is indexed by package provenance for WebAssembly. Never-driven release executes exact callback, endpoint, and body cleanup before Allocation release; completion executes callback then endpoint cleanup before release. New evaluator/WebAssembly tests make exact stored body and endpoint Drop hooks observable. |
| External relinquishment and later Eligible redrive were not executable in this slice. | High | Not accepted as a Layer 2 implementation defect. This change explicitly precedes `add-external-wake-parking`; that slice owns the only external-park producer and the Registering/Latched/Dormant/Notifying/Eligible transition policy. Layer 2 retains the affine `onSuspend` MIR seam, lifecycle admission model, readiness plan, and canonical ownership transitions. Inventing a producer or wake policy here would pull the next slice forward. |
| Package-specific canonical `SuspensionOwnership` assertions were not isolated in its unit test. | Medium | Verification-gap observation retained. Package cleanup matrices are exercised in `ExecutionPackage.test.ts`, and the canonical artifact remains the sole encoded ownership authority; no divergent model or behavior defect was demonstrated. |
| Initializer-provenance tests did not independently perturb every MIR provenance field. | Medium | Verification-gap observation retained. Unit tests perturb target, size, alignment, and package identity, while `MirVerification` compares result/body/endpoint/callback/suspension facts exactly. No acceptance hole was demonstrated. |
| The implementation report still described the superseded parked state. | Medium | Verified and fixed by this resumed report and exact gate ledger. |
| Execution provenance expanded the actor still named `LocalSharedAllocationProvenance`. | Medium | Architectural follow-up retained. It is canonical allocation-origin analysis shared by both consuming constructors, but the name is narrower than its role. No correctness or privilege defect was demonstrated in this slice. |
| The OpenSpec prose calls the source operation `drive`, while the generated internal inventory tag is `executionDrive`. | Low | Accepted realization refinement. Source exposes `Intrinsic.drive`; the collision-resistant internal sealed catalog identity remains `ExecutionDrive`/`executionDrive`, and no extra source-callable privilege exists. |
| Finite Effect behavior, typed failure/requirement channels, target-independent package layout, minimal intrinsic privilege, and no-forward wake policy conform. | — | No finding. Focused and full gates pass, and the lens traces found no actor-name recognition or Layer 3 wake/parking implementation. |

## Minimal-privilege audit

Semantic, HIR, MIR, evaluation, and backend selection use sealed intrinsic identities. The compiler
surface remains the three target-neutral package operations; Allocator selection, safe construction,
and policy remain ordinary source. No Scheduler, ready queue, Timer, Deferred, Coroutine, Wake, or
actor-spelling recognition was introduced by this slice.

## Final re-audit and archive verification (2026-08-24)

This report closes against the complete five-change SLP-0001 implementation DAG after integrating
`origin/main` at merge commit `31bdfec`. Repeated independent language/specification,
architecture/standards, and packaging/evidence reviews found no remaining significant defect at
source checkpoint `444b0d9`. The final review confirmed exact package provenance, cleanup ordering,
sealed intrinsic admission, callback-first fatal handling, and package export/release coverage.

The implementation and language-documentation checkpoint `9b4a311` passed `pnpm typecheck` (24/24
tasks), `pnpm exec biome check .` (991 files), `pnpm test` (28/28 tasks, including 220 compiler
files / 2,151 tests and the native differential suite), `pnpm check` (42/42 Turbo tasks plus 16/16
script tests), and `pnpm release:candidate` (9/9 validations). All tasks are complete and no
significant audit finding remains open.
