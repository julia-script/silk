# Implementation report: prove-independent-execution-separation

Status: **PARKED — semantic repair budget exhausted by a new native lifecycle blocker**

## Implemented pressure surface

- Added connected ordinary-source TaskStore/ReadyInbox ownership with fixed endpoints,
  removal-before-drive, no Shared access across drive/callback invocation, two exact lazy body
  representations, Deferred-shaped publication, and final result `42`.
- Added bounded alternate-owner, timer/reactor, dormant cancellation, selective-ready/tombstone,
  construction rollback, and post-publication allocation-failure fixtures. All remain explicitly
  test-local and non-canonical.
- Extended `LocalSharedPressure.test.ts` with evaluator/direct-Wasm lifecycle traces, balanced
  allocation evidence, every exercised construction ordinal, provider-cause detachment diagnostics,
  unowned-entry diagnostics, structural pay-for-use inventories, actor renaming, and deterministic
  direct-Wasm emission.
- Extended the designated native differential corpus with success, cancellation, alternate-owner,
  timer, selective-ready, and construction-boundary programs. Native-dependent task boxes remain
  open because the complete differential corpus is not green.
- Recorded the selected pressure guarantees, rejected smaller model, privilege inventory, and
  deferred language boundaries in `findings.md`.

## Root-cause repair history

1. **Wasm recursive cleanup authority:** nested aggregate/union cleanup walks ignored
   `ExecutionCleanup` and `WakeCleanup`. Explicitly dropping a stored dormant Execution therefore
   emitted no cancellation. Package-aware recursive cleanup now realizes both plans; the minimized
   post-`onSuspend` dormant-cancel and bounded alternate-owner evaluator/Wasm regressions pass.
2. **Wasm inherited borrow root classification:** a `BeginLoan` whose root was an `EffectBorrow`
   parameter was incorrectly planned as frame-owned storage. Nested ordinary Shared capture then
   failed with `Wasm frame ... lost address-taken root`. `WasmMemory` excludes inherited borrows
   from frame roots, and `WasmBackend` forms the loan from the inherited borrow-pointer lane. The
   seven-way pay-for-use matrix passes and Shared direct/nested cases retain `LocalExecution` with
   zero independent packages.
3. **Native exact package capability dispatch:** native Wake cleanup enumerated every Execution
   package, including plans without Wake control, and drive enumerated same-result packages while
   assuming all had continuation storage. Wake cleanup now selects only readiness-bearing plans;
   direct packages have a distinct Initial-only drive path and do not fabricate a continuation
   segment. The minimized connected mixed direct/external owner passes the designated native
   harness.

After repair 3, two additional new corpus cases terminate the native process by signal while the
evaluator and direct Wasm complete: `independent-execution-separation-dormant-cancel` expects `0`,
and `independent-execution-separation-coroutine` expects `123`. Both exercise dormant destruction
with retained Wake/Shared state; this is a distinct native cancellation/resumption lifetime cause,
not another package-selection compile failure. The SLP-5 three-root-cause limit therefore requires
parking before another semantic edit.

## Verification history

- Checkpoint `021ae09`: focused local-shared/Execution pressure suite **PASS**, 8 tests; dormant
  cancellation and recursive Wasm cleanup regression **PASS**.
- Checkpoint `9cbb79e`: `LocalSharedPressure.test.ts` **PASS**, 14/14 tests in 22.95s; compiler
  typecheck **PASS** after mechanical toolchain identity generation; targeted Biome **PASS** after
  formatting.
- Full designated native differential, first attempt — **FAIL** after 85.35s with `Wake package
  lacks control state`. This localized to the connected mixed-package program.
- Temporarily filtered unchanged native harness, connected-owner after repair 3 — **PASS**, 1/1,
  native exit and evaluator result `42`. The diagnostic-only filter was removed.
- Temporarily filtered unchanged native harness, first-activation after repair 3 — **PASS**, 1/1,
  native exit and evaluator result `20`.
- Temporarily filtered unchanged native harness, coroutine — **FAIL**, native status `null`
  (terminated by signal) versus evaluator `123`.
- Temporarily filtered unchanged native harness, dormant-cancel — **FAIL**, native status `null`
  (terminated by signal) versus evaluator `0`.

The formal hard-gate sequence did not start because the required focused native evidence is red.
No conformance pass was started; the parent task owns it only after initial gates are green.

## Attempt budget and task state

Semantic root-cause fixes used: **3/3**. Generated toolchain refreshes, formatting, and TypeScript
test narrowing were mechanical and are not semantic attempts. OpenSpec is **12/21** after the
source-name inventory and findings report; native/all-engine, documentation, and gate-dependent
boxes remain unchecked.

