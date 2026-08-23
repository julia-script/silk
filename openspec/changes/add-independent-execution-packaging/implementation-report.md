# Implementation report

## Result

Parked on hard gates after the bounded three-root-cause fix budget. Fourteen of fifteen OpenSpec
tasks are complete. The remaining gate task, 4.2, is intentionally unchecked because the final
full test gate regressed the existing finite Effect-join behavior.

The implemented slice adds exact target execution-package planning and provenance, consuming
construction from owned allocation, take-once drive callbacks, evaluator logical-root ownership,
nested transfer handling, cleanup and resource accounting, direct WebAssembly execution, and the
ordinary-source `silk/execution` wrapper. It does not add Layer 4 wake, park, or implicit-entry
policy.

## Focused evidence

The focused command passed five files and 47 tests:

```text
pnpm --filter @silk-effect/compiler exec vitest run \
  test/ExecutionPackage.test.ts \
  test/EffectSuspensionWasm.test.ts \
  test/IntrinsicCatalog.test.ts \
  test/Layout.test.ts \
  test/SuspensionOwnership.test.ts \
  --reporter=dot

Test Files  5 passed (5)
Tests       47 passed (47)
```

The execution-package cases cover direct, nested, and external package planning; provenance
mismatch and overflow; Initial and Eligible drive entry; fatal Dormant and Notifying entry;
independent roots; never-driven and completion cleanup for affine body, endpoint, selected callback,
and unselected callback captures; allocation refusal; nested evaluator and WebAssembly transfer;
fatal stack exhaustion without a reified failure or extra allocation; non-LIFO first roots; and
typed failure reification as `Result` on the evaluator and WebAssembly.

The minimal-compiler-privilege audit found only sealed execution intrinsic tags in semantic, HIR,
MIR, evaluation, and backend dispatch. Allocator policy and wake/parking actors remain ordinary
source or later-layer concerns.

## Hard-gate history

Required order was `pnpm typecheck`, `pnpm exec biome check .`, `pnpm test`, `pnpm check`, then
`pnpm release:candidate`.

1. Initial `pnpm typecheck` failed in the downstream inspector because new cleanup, MIR operation,
   and bootstrap value variants were not rendered exhaustively. Root-cause fix 1 added the four
   inspector renderers. Its focused typecheck then passed.
2. The next `pnpm typecheck` passed (24/24 tasks). `pnpm exec biome check .` failed on import order,
   a type-only import, and the stale represented-layout parameter. Root-cause fix 2 organized the
   imports and removed the unused parameter. A clean gate restart passed typecheck and Biome.
3. `pnpm test` then failed the standard-library documentation policy because
   `silk/execution::make` lacked the required section boundary. Root-cause fix 3 added `# Details`,
   regenerated `Stdlib.generated.ts`, and generated the standard-library and diagnostics docs. An
   intermediate run exposed the missing generated pages and was treated as the same documentation
   completeness root cause, not a fourth fix. The following clean restart passed typecheck and
   Biome.
4. The final `pnpm test` gate failed: 2 compiler files failed, 215 passed; 10 compiler tests failed,
   2,090 passed. `BootstrapEvaluation.test.ts` reported corpus `finite-effect-join` as `Blocked`
   where `Completed` was expected, and nine `EffectJoin.test.ts` cases showed the same finite-join
   regression family. This is a fourth distinct gate root cause after the allowed three fixes, so
   the change is parked without another implementation attempt.

`pnpm check` and `pnpm release:candidate` were not run because the preceding full test gate failed.

## Conformance ledger

The three-lens conformance pass was not started. The worker contract runs it only after hard gates
pass, and this change reached the bounded gate stop first. Therefore there are no lens findings to
accept, reject, or fix.

Implementation-time findings are recorded below so the parked handoff does not lose evidence.

| Claim | Severity | Disposition and evidence |
| --- | --- | --- |
| A consumed exact take-once callback could retain affine captures without cleanup. | High | Verified and fixed. Evaluator and WebAssembly completion tests assert exact selected and unselected callback capture release. |
| A never-driven package could retain body captures. | High | Verified and fixed. Never-driven evaluator and WebAssembly tests assert exact body and endpoint release. |
| WebAssembly package execution omitted executable-environment lane storage. | High | Verified and fixed. Direct and nested WebAssembly package tests pass with captured executable values. |
| The downstream inspector was non-exhaustive for the new cleanup, MIR, and value variants. | Medium | Verified and fixed; inspector typecheck and the subsequent repository typechecks pass. |
| The new source wrapper did not initially satisfy the standard-library documentation and generated-artifact policy. | Medium | Verified and fixed; the policy progressed past documentation and the final typecheck/Biome restart passed. |
| Existing finite Effect joins now block instead of completing. | High | Verified and parked. The full suite reports 10 failures across `BootstrapEvaluation.test.ts` and `EffectJoin.test.ts`. The represented-executable layout broadening is a plausible shared cause, but the bounded stop prevents an unverified fourth fix. |
| External suspension entry is represented at the drive seam without introducing Layer 4 wake or parking policy. | Medium | Accepted scope boundary. The layer preserves the package/drive contract and callback ownership while deferring wake/park actors exactly as the change non-goals require. |

## Next action

Resume `add-independent-execution-packaging` with a fresh gate-fix budget, localize the finite
Effect-join regression end to end (starting at represented executable layout selection), restore
the existing join outcomes without weakening exact package planning, then rerun every hard gate in
repository order. Only after all hard gates pass should the single three-lens conformance pass run.
