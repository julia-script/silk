# Implementation report: add-independent-execution-engine-parity

Status: **complete; conformance fixes and final hard gates passed**

## Scope

This layer consumes the target-neutral package, drive, park, Wake, affinity, and cleanup contracts
from the first three SLP-0001 changes. It owns complete transition authority and evaluator, native,
and direct-Wasm realization. Public API remains ordinary `silk.execution` source over the sealed
`Intrinsic` primitives; no Scheduler, Timer, reactor policy, transfer, or atomic primitive is added.

## Checkpoint 1: transition authority and evaluator continuation

- Added `ExecutionTransition`, the backend-independent composition of `ExecutionLifecycle` and
  `WakeCell`. It assigns stable package/root/generation identities, validates complete lifecycle and
  cleanup edges, exposes deterministic representation-free inspection, and supplies private state
  tags without fixing an ABI.
- Added negative transition tests for dormant drive, duplicate Wake, premature registration reuse,
  DestroyPending cleanup dominance, and late cancelled-Wake final release.
- Changed external-park reachability from intrinsic actor/name spelling to the closed
  `ExecutionPark` builtin-operation identity, resolving the Layer 3 Medium finding.
- Extended the evaluator's existing independent-call machine with an explicit per-execution retained
  stack/transfer record. `ExecutionPark` now registers one Wake, retains its guard, relinquishes the
  independently rooted machine, and resumes the same generator/frame stack only after eligibility.
- Implemented evaluator Wake latch, post-suspension notification, Eligible publication, guard drop
  immediately before resume, completion, dormant/late-Wake cleanup, and deferred Notifying cleanup.
  Canonical transition trace events use allocation-order package/root identities, never object
  identity.

Tasks 1.1 and 1.3 are complete. Task 1.2 remains open until callback/provenance/loan-negative MIR
fixtures are added. Evaluator tasks remain open until the full ordering/cleanup matrix and two-root
non-LIFO fixture pass.

## Checkpoint 2: canonical external relay and direct-Wasm resume

- Final MIR now retains an `ExecutionPark` relay as an explicit external transfer origin. The prior
  origin-only finalization discarded the otherwise valid park frame and caused direct Wasm to omit
  its suspension runtime entirely. Orphan verification now treats only the closed `ExecutionPark`
  operation as that origin; ordinary suspendable calls still require a reachable transfer source.
- Coroutine verification accepts the park guard's purpose-bound success release and excludes that
  consumed guard from restored payload slots. Zero-lane guards correctly require no frame release.
- Direct Wasm persists the independent continuation head in its exact execution package, restores
  it only for a later Eligible drive, and relinquishes without running the source continuation.
  Registration receives the sealed Wake pointer, latch occurs during registration, and notification
  runs only after `onSuspend` returns.
- Wasm readiness delivery reconstructs the exact represented callback and hidden environment from
  package-layout facts and invokes it through the stored endpoint borrow before publishing Eligible.
  It does not recognize a source actor or declaration spelling.
- Coroutine frames now use deterministic fixed-size slots with a non-LIFO-safe free list. A later
  drive of one root cannot rewind or overwrite another parked root's continuation frame.
- The evaluator/Wasm latched-resume acceptance program agrees on result `42`, one stable logical
  root, and `Initialize, Drive, Register, Latch, RetainGuard, Notify, Eligible, Resume, Drive,
  Complete`.

Native realization, the complete destruction/cancellation matrix, two-root alternating acceptance,
and reactor availability remain open; task 3.2 therefore remains unchecked despite the first
working direct-Wasm external-resume path.

## Checkpoint 3: independent native roots and reverse-order parity

- Native coroutine frames are independently allocated, alignment-correct roots with a bounded
  thread-local total-byte reservation and explicit pop. Retained executions therefore no longer
  depend on a segmented LIFO rewind point shared with another root.
- Native calls preserve the distinct external-relay status through suspendable callers. Translating
  that status to an ordinary nested-child transfer caused the driver to invoke an uninitialized
  child thunk; preserving status `2` makes the execution owner, rather than the ambient caller, the
  next authority.
- Native Execution initialization, drive, park, Wake, completion, continuation restoration, frame
  cleanup, package cleanup, and late-Wake allocation discharge lower from canonical package and
  suspension facts. A linked single-root signal/resume probe exits `42`, and the designated native
  corpus' order-sensitive two-root non-LIFO case agrees with evaluation at `240` (`222` would
  expose swapped root restoration while remaining inside native process-exit range).
- Direct Wasm now clears the transient transfer-tail cell when a new independent-root drive begins.
  Without that reset, a detached first root left the shared relay tail pointing into its retained
  chain and a second root appended into it. The execution-owned continuation head remains the sole
  retained authority; reverse-order two-root evaluation and Wasm now agree at `42`.
- The designated native corpus includes `independent-execution-non-lifo`; the ordinary focused
  external-parking suite verifies evaluator/Wasm reverse completion order, while a filtered run of
  the unchanged designated native differential harness verified the native case.

The complete cancellation/DestroyPending matrix, multiple same-result package dispatch, reactor
availability evidence, and full gates remain open, so tasks 3.1, 3.2, and 4.1 remain unchecked.

## Checkpoint 4: cleanup matrix, exact package dispatch, and local reactor

- Evaluator and native cancellation now distinguish a consumed/latched Wake from an outstanding
  retained Wake. Destroying the Dormant execution removes its continuation and endpoint authority
  immediately, while the registration allocation remains until the late cancelled Wake consumes
  the final authority and emits one canonical `Release`.
- Reentrant endpoint destruction enters DestroyPending, cleans retained frames and the package only
  after notification returns, and never publishes Eligible. The same callback-return boundary is
  used by evaluator, native, and direct Wasm.
- Native and direct Wasm select initialization, Park wake-control storage, Wake notification, and
  Drive code by the exact package ordinal stored in the runtime package. Two packages with the same
  result type but different body layouts therefore cannot alias through a name- or result-keyed
  dispatch path.
- The negative MIR fixture forges an initializer plan, a completion callback access, and a Wake
  access; verification rejects each as `InvalidExecutionOperation` before lowering. Together with
  the transition-unit cases, this covers invalid predecessors, duplicate authorities, premature
  generation reuse, DestroyPending endpoint ordering, and retained-loan/callback contracts.
- The differential matrix now includes repeated park generations, Eligible destruction, late
  cancelled Wake, reentrant notification destruction, and reified typed failure after a park.
  Evaluator and direct Wasm agree at every case; all target-specific boundaries are also entries in
  the unchanged designated native differential corpus.
- An ordinary same-thread source reactor retains, extracts, and consumes Wake. Renaming its actor
  and poll operation does not alter analysis. Native and Wasm artifacts contain no atomics, workers,
  work-stealing, scheduler, or compiler-known timer path, and an unsupported target remains
  explicitly unavailable.

Tasks 1.2, 2.1-2.3, 3.1-3.3, and 4.1-4.3 are complete. Nested/fatal regression, repeated artifact
determinism, and repository gates remain open until the formal gate sequence.

## Required three-lens conformance pass and consolidated fix

The language/SLP, OpenSpec/task, and architecture/minimal-privilege lenses completed exactly once.
Their independently reported Critical/High findings converged and were verified locally before the
single consolidated fix pass:

- **Critical — transition authority was test-only:** accepted. `Mir.Module` now carries one complete
  canonical `ExecutionTransition.Authority` per exact execution-package plan. Lowering constructs
  it, `MirVerification` rejects missing, reordered, forged-generation, illegal-predecessor, and
  early-cleanup edges, and `MirEncoding` prints logical state/root/generation/cleanup edges.
  Evaluator drive/register/guard/relinquish/wake/notify/cancel operations consume this actor; native
  and Wasm select lifecycle tags from it after MIR verification.
- **Critical — Wasm cancellation freed live package storage before a late Wake:** accepted. Dormant
  destruction marks the wake cell Cancelled before value cleanup, retains Allocation while an
  external Wake exists, and makes the final Wake consume/drop release Allocation exactly once.
  Latched destruction during `onSuspend` enters DestroyPending until that callback returns. A
  strengthened fixture keeps endpoint state alive elsewhere: the old path observably returned
  `1042` by invoking the cancelled endpoint; the repaired evaluator and Wasm both return `42`.
- **High — Wasm omitted WakeCleanup and continuation-value cleanup:** accepted. `WakeCleanup` has a
  dedicated package-aware lowering. Every retained resume id has a typed cleanup thunk that loads
  affine lanes from the separate coroutine-frame memory, runs the existing cleanup plan including
  hooks and reclaim, and only then returns the slot to the non-LIFO free list.
- **High — evaluator cancellation leaked retained machines and stack accounting:** accepted. Cleanup
  removes the parked machine, walks canonical affine frame slots once, emits `Cleanup`, completes
  frame trace records, and subtracts each charged frame before a later execution can reserve it.
- **High — evidence gaps:** accepted. Negative MIR authority forgeries cover incomplete paths,
  generation drift, and endpoint cleanup before a live invocation. The non-LIFO program returns the
  order-sensitive value `240`; direct Wasm emission is repeated byte-for-byte; evaluator/Wasm cover
  latched `onSuspend` destruction, illegal Dormant drive, and independent-root stack exhaustion
  before callbacks. The illegal-drive native corpus case exposed LLVM `unreachable` as optimizer
  undefined behavior, so native now branches to the existing real `llvm.trap` block.
- **Medium — duplicate `Park` trace vocabulary:** accepted. Guard retention and actual suspension
  handoff are distinct `RetainGuard` and `Relinquish` events; cancellation teardown is an explicit
  `Cleanup` event.

Consolidated focused evidence: compiler source/test TypeScript **PASS**; transition, external Wake,
and execution-package suites **PASS**, 3 files / 35 tests; full designated native differential
corpus **PASS**, 1 test / all programs, 120.16s. No second conformance pass was run.

## Gate closure

- The complete existing nested/LIFO, evaluator execution-stack exhaustion, native bounded-stack,
  and direct-Wasm bounded-stack suites pass unchanged. Fatal paths remain traps outside the typed
  outcome channel and run no newly introduced cleanup or outcome callback.
- Repeated native artifacts, direct-Wasm golden/byte determinism, fresh-process artifact canaries,
  canonical MIR encoding, and deterministic runtime label/package selection all pass in the full
  repository suite. Task 3.5 relies on these existing global determinism canaries rather than adding
  a redundant per-feature fresh-process test.
- `pnpm typecheck` first exposed two distinct closure causes. The inspector's exhaustive renderers
  needed representation-free cases for `ExecutionTransition` and the internal
  `ExecutionRelinquished` control result. The verifier-negative fixture also needed its deliberately
  malformed operation cast isolated at the test rewrite boundary and exact trace narrowing. Both
  were localized and verified; no third root-cause repair was used.
- After those fixes, the pre-conformance formal sequence passed in order: focused regression, root
  typecheck, full Biome, the full repository test graph, repository check, and release candidate.
  The required three-lens conformance review and its single consolidated fix pass then completed.
- The first post-conformance `pnpm test` attempt exposed the final bounded root cause: typed Wasm
  cancellation cleanup thunks were generated for every ordinary suspension module even when it
  had no execution package. Those helpers asked the normal callable ABI for frame-only semantic
  types and failed with `Wasm backend lost a calling shape`. The repair restricts these thunks to
  modules with execution-package cleanup authority and retains only affine payload types in each
  helper. Ordinary suspension and external-Wake suites then passed together before the full rerun.
- The complete post-conformance sequence passed after that repair. All 15 OpenSpec tasks are
  complete and no Critical, High, or gate blocker remains.

## Verification history

- Initial focused command could not resolve unbuilt workspace package `@silk-effect/llvm/Bitcode`;
  building `@silk-effect/llvm` relocalized the same prerequisite to `@silk-effect/wasm/Binary`.
  Building both workspace packages supplied fresh-worktree outputs; this was environment setup, not
  a semantic repair.
- Focused transition/Wake/package/external-parking sequence — **PASS**, 4 files / 33 tests.
- Compiler source and test TypeScript checks — **PASS**.
- Targeted Biome check over all changed files — **PASS** after mechanical formatting.
- Post-checkpoint focused regression — **PASS**, 4 files / 27 tests, including evaluator-to-Wasm
  latched external resume and existing synchronous Execution package completion/failure behavior.
- Generated toolchain identity was refreshed after compiler source changed; direct TypeScript checks
  then passed. The formal hard-gate loop has not started.
- Native/Wasm milestone TypeScript check — **PASS**.
- External parking focused regression — **PASS**, 1 file / 11 tests, including deterministic native
  never-driven cleanup and reverse-order two-root evaluator/Wasm parity.
- Designated native differential harness, temporarily filtered to the newly committed corpus entry
  without changing harness semantics — **PASS**, 1 program; source result and native exit both `42`.
- Targeted Biome check over the native/Wasm milestone — **PASS** after mechanical formatting and
  removal of unused destructured bindings.
- Toolchain integrity generation and check after native source additions — **PASS**.
- Full designated native differential corpus after cleanup, provenance, reactor, generation, and
  typed-failure additions — **PASS**, 1 harness test, 126.74s; interpreter and native outcomes agree.
- Closure focused regression — **PASS**, 4 files / 41 tests, including all target-neutral transition,
  Wake-cell, execution-package, evaluator, and direct-Wasm external-parking cases.
- Formal focused regression — **PASS**, 4 files / 41 tests; designated native differential corpus
  **PASS**, 1 test / all corpus programs.
- `pnpm typecheck` — attempt 1 **FAIL**, inspector exhaustive consumers omitted the new trace/control
  variants; attempt 2 **FAIL**, verifier-negative test typing and trace narrowing; attempt 3
  **PASS**, 24/24 tasks.
- `pnpm exec biome check .` — **PASS**, 989 files.
- `pnpm test` — **PASS**, 28/28 tasks in 12m05.959s; compiler parallel suite 220 files / 2,131 tests,
  designated native differential 1/1, and every downstream package suite passed.
- `pnpm check` — **PASS**, 14/14 build tasks, 42/42 cached typecheck/test tasks, and 16/16 script
  tests.
- `pnpm release:candidate` — **PASS**, release-candidate validation 1 file / 9 tests.
- Post-conformance attempt 1: focused regression **PASS**, 4 files / 36 tests; root typecheck
  **PASS**, 24/24 tasks; full Biome **PASS**. `pnpm test` **FAIL**, compiler parallel suite reported
  2 files / 8 assertions failing from the shared eager Wasm frame-cleanup helper cause; Turbo
  stopped at 16 successful / 21 total tasks in 4m08.657s. Non-interactive localization reproduced
  the cause in `EffectSuspensionWasm`, 7 assertions, before its bounded bail.
- Final repair localization: compiler TypeScript **PASS**; `EffectSuspensionWasm` plus
  `ExternalWakeParking` **PASS**, 2 files / 27 tests.
- Final post-conformance sequence: focused regression **PASS**, 4 files / 36 tests in 126.16s;
  `pnpm typecheck` **PASS**, 24/24 tasks; `pnpm exec biome check .` **PASS**, 989 files;
  `pnpm test` **PASS**, 28/28 tasks in 12m56.015s, including compiler 220 files / 2,134 tests and
  native differential 1/1; `pnpm check` **PASS**, 14/14 build tasks, 42/42 typecheck/test tasks, and
  16/16 script tests; `pnpm release:candidate` **PASS**, 1 file / 9 tests in 32.32s.

## Attempt budget

Hard-gate root-cause fixes used: **3/3**. The final slot was the eager Wasm frame-cleanup thunk
generation described above. Fresh-worktree build prerequisites and normal generated identity
refreshes were mechanical setup, not additional semantic repairs.

## Final re-audit and archive verification (2026-08-24)

This report closes against the complete five-change SLP-0001 implementation DAG after integrating
`origin/main` at merge commit `31bdfec`. Repeated independent language/specification,
architecture/standards, and packaging/evidence reviews found no remaining significant defect at
source checkpoint `444b0d9`. Later audit repairs strengthened evaluator, WebAssembly, and native
parity evidence without introducing a compatibility path or broadening compiler privilege.

The implementation and language-documentation checkpoint `9b4a311` passed `pnpm typecheck` (24/24
tasks), `pnpm exec biome check .` (991 files), `pnpm test` (28/28 tasks, including 220 compiler
files / 2,151 tests and the native differential suite), `pnpm check` (42/42 Turbo tasks plus 16/16
script tests), and `pnpm release:candidate` (9/9 validations). All tasks are complete and no
significant audit finding remains open.
