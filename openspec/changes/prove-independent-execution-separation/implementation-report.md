# Implementation report: prove-independent-execution-separation

Status: **DONE — 21/21 TASKS AND ALL POST-CONFORMANCE GATES GREEN**

## Implemented pressure surface

- Added connected ordinary-source TaskStore and ReadyInbox ownership with fixed endpoints,
  removal-before-drive, and no Shared access across drive or callback invocation. Two exact lazy
  body representations enter one homogeneous owner store before the owner selects first activation.
- Added a Deferred-shaped waiter and producer, a fallibly prepared same-thread timer/reactor, a
  timer cancellation path, a bounded Coroutine-shaped alternate owner, selective readiness, and
  stale-identity consumption. These actors remain test-local and non-canonical.
- Added deterministic construction-quota and post-publication-failure evidence. Every exercised
  evaluator and direct-Wasm construction ordinal balances acquired and released allocations and
  publishes no partial task. Native covers the designated boundary ordinals through the shared
  differential corpus.
- Added structural pay-for-use evidence across ordinary direct, ordinary nested, explicit direct,
  explicit nested, explicit external-park, and local Shared capture variants. Actor renaming keeps
  semantic facts, normalized MIR, evaluator results, Wasm results, and intrinsic inventories
  equivalent apart from source identity.
- Updated the language and standard-library documentation for hidden representation, Initial
  activation, nested transfer, external parking, typed package admission, fatal later stack growth,
  and complete inert-package retention behind a forgotten cancelled Wake.

## Runtime repairs

The preserved implementation required three bounded repairs before the resume:

1. **Wasm recursive cleanup authority.** Aggregate and union cleanup walks now realize
   `ExecutionCleanup` and `WakeCleanup`. Dormant destruction therefore cancels nested executions,
   releases retained frame values, and preserves late-Wake authority.
2. **Wasm inherited borrow-root classification.** A `BeginLoan` rooted in an `EffectBorrow`
   parameter is no longer planned as frame-owned storage. The backend now forms that loan from the
   inherited borrow-pointer lane, which preserves the local Shared nested-only pay-for-use path.
3. **Native exact-package capability dispatch.** Wake cleanup selects only readiness-bearing
   packages. Direct packages use an Initial-only drive path and do not fabricate continuation
   storage.

The fresh resume used two of its three distinct root-cause repairs:

1. **Native address-taken root synchronization.** Native `WritePlace` updated only SSA or mutable
   storage for a whole-root replacement. A later call reloaded stale address-taken storage and
   resurrected a freed dormant Execution. `NativePlaceOperation` now synchronizes address roots
   after whole-root and selected-place writes. The dormant-cancel and Coroutine native witnesses
   now return `0` and `123` instead of terminating by signal.
2. **Native exit observation.** The differential harness compared a signed evaluator result to a
   POSIX process status. Process status exposes the low eight bits, so expected values now use
   `result & 0xff`. The construction-failure `-100` witness therefore compares to `156` without
   changing language or runtime semantics.

The remaining repair slot was consumed after conformance by completing the sealed-storage migration
across checked/formatting surfaces. See the post-conformance gate history below.

## Single conformance pass dispositions

The parent-owned language/SLP, OpenSpec/task, and architecture/minimal-privilege lenses completed
exactly once. The single consolidated fix pass accepted and closed all seven verified findings:

1. **Critical — recursive Wasm cleanup authority alias.** Package authority now remains in a
   stable local distinct from the recursive frame cursor. A nested aggregate/union dormant package
   proves exact body, frame, guard, endpoint, package, and late-Wake reclamation without using freed
   storage.
2. **High — shallow timer witness.** The timer fixture now has a Scheduler-shaped outer Execution,
   a detached timer-owned child, an explicit outer result owner, fallible pre-park preparation,
   sibling progress, child-to-owner readiness, and cascading cancellation of both waits. Its late
   timer Wake consumes without publication or redrive in evaluator, Wasm, and the native corpus.
3. **High — unrelated post-publication allocation.** The failure now occurs while inserting a real
   Deferred/Fiber join waiter through its declared allocation channel. It emits no register,
   retain, relinquish, or park transition and leaves the published Initial task valid.
4. **High — MIR-only pay-for-use evidence.** All seven configurations now compare deterministic
   structured direct-Wasm and native linkage/runtime inventories for runners, packages, drive,
   dormant continuation, Wake, notification, and thread/atomic support. Owner drive remains
   NonParking; no byte, offset, or instruction count is asserted.
5. **High — incomplete actor-neutrality witness.** Scheduler, Deferred, timer, Coroutine,
   allocator/provider, and safe Execution-wrapper spellings are renamed together. Normalized
   semantic and MIR evidence, evaluator/Wasm results, emitted target inventories, and designated
   native outcomes remain equivalent. A machine-falsifiable compiler-phase source inventory rejects
   those actor spellings.
6. **High — ordinary allocation-policy privilege.** The compiler no longer recognizes
   `silk/core.OutOfMemoryError`, `Allocator`, or `SystemAllocator` to select storage acquisition.
   The primitive reports sealed `Intrinsic.StorageFailure`; ordinary `silk/core` source translates
   it into public allocation policy. Direct primitive fixtures use the sealed failure or an
   ordinary source adapter, and unused canonical identities were removed.
7. **High — incomplete cleanup/rollback observability.** Source-observable probes now distinguish
   body, frame, registration guard, endpoint/port, late Wake, final package authority, and
   construction rollback ordering. This witness exposed and fixed a second Wasm ownership error:
   after activation, continuation frames—not the obsolete body environment—own captured body
   values, so dormant cancellation no longer double-drops them.

No second conformance review was run.

## Focused verification

- `pnpm exec vitest run packages/compiler/test/ExternalWakeParking.test.ts packages/compiler/test/LocalSharedPressure.test.ts`
  — **PASS**, 2 files and 34 tests, 24.10s.
- `pnpm exec vitest run packages/compiler/test/DriverNativeAcceptance.test.ts` — **PASS**, 1 file
  and 1 differential-corpus test, 143.18s.
- Timer evaluation records the exact success and cancellation transition sequences. It also proves
  sibling progress before reactor polling, polling before endpoint notification, balanced
  allocation release, evaluator result `42`, and direct-Wasm result `42`.
- After the consolidated pass, `LocalSharedPressure.test.ts` passed **14/14** in 35.21s; the
  storage-policy migration suite passed **147/147**; and the native differential corpus, including
  four renamed-policy executions, passed **1/1** in 139.87s.
- `pnpm --filter @silk-effect/compiler documentation:policy` — **PASS**, 46 modules and no
  violations.
- `pnpm --filter @silk-effect/compiler documentation:check` — **PASS**.
- `pnpm --filter @silk-effect/compiler documentation:examples` — **PASS**, 54 of 54 doctests after
  building the doctest package.

## Hard gates

The initial hard-gate sequence ran once in the required order:

1. `pnpm typecheck` — **PASS**, 24 successful Turbo tasks in 11.956s. Existing TypeDoc warnings
   remained warnings.
2. `pnpm exec biome check .` — **PASS**, 989 files checked with no fixes.
3. `pnpm test` — **PASS**, 28 successful Turbo tasks in 10m59.8s. The compiler parallel suite passed
   220 files and 2,146 tests; the native differential corpus passed 1 of 1.
4. `pnpm check` — **PASS**, 42 cached Turbo tasks and 16 repository script tests.
5. `pnpm release:candidate` — **PASS**, release-candidate validation passed 9 of 9 tests in 28.00s.

There were no gate retries and no pre-existing red gate. A first documentation-example invocation
before the formal gate sequence lacked `packages/doctest/dist/bin.js`; building the declared
doctest package resolved that mechanical prerequisite, after which all 54 examples passed.

## Post-conformance hard-gate history

The post-conformance sequence exhausted the resumed repair budget and is parked:

1. **Attempt 1.** `pnpm typecheck` passed 24/24 tasks in 12.825s. `pnpm exec biome check .`
   stopped on two formatter-only diffs in `LocalSharedAllocationProvenance.ts` and
   `EffectRuntime.test.ts`. Those files were formatted as part of the remaining sealed-storage
   migration repair.
2. **Attempt 2.** `pnpm typecheck` passed 24/24 tasks in 11.703s and Biome passed 991 files in
   672ms. `pnpm test` then stopped after 3m32.694s with 1 of 2,146 compiler tests failing: the
   checked intrinsic inventory still described `StorageAcquire` as failing with
   `OutOfMemoryError`. That fixture and the resulting toolchain digest were the same incomplete
   sealed-storage migration cause; the focused `IntrinsicCatalog` suite then passed 9/9.
3. **Attempt 3.** `pnpm typecheck` passed 24/24 tasks in 7.143s and Biome passed 939 files in 654ms.
   `pnpm test` stopped after 4m58.436s with 1 of 2,146 compiler tests failing: the actor-neutrality
   witness timed out at 60,000ms under the full parallel compiler suite. The same test passes
   focused in 35.21s, so this is a distinct test-cost/full-suite-load cause rather than a semantic
   mismatch. The resume repair budget was already exhausted; no timeout was raised and the layer
   failed closed.

`pnpm check` and `pnpm release:candidate` were not run after conformance because the required test
gate did not pass. No second conformance review or additional repair pass was started.

## Gate-only resume

The fresh gate-only resume localized the timeout to four independent actor-neutral fixtures sharing
one 60-second test deadline. Under full-suite load their aggregate exceeded the deadline even though
each fixture remained individually cheap. The existing file now gives each fixture its own test
boundary while preserving the same semantic, normalized MIR, evaluator, direct-Wasm, native
artifact, and inventory assertions. Each snapshot, lowered MIR, renamed source, and emitted artifact
is materialized once per target. The compiler-phase spelling inventory is a separate synchronous
test and no timeout was raised.

- Focused `LocalSharedPressure.test.ts`: **PASS**, 18/18 tests in 40.12s. The four actor-neutral
  cases completed in 4.313s, 5.696s, 3.281s, and 4.147s.
- Exact full compiler context, `pnpm --filter @silk-effect/compiler test:parallel`: **PASS**, 220
  files and 2,150 tests in 201.36s.

This is gate-resume root-cause fix 1/3. No further repair was needed.

The fresh gate-only hard-gate sequence then ran once in the required order:

1. `pnpm typecheck` — **PASS**, 24/24 Turbo tasks in 5.597s. The existing 31 TypeDoc warnings
   remained warnings.
2. `pnpm exec biome check .` — **PASS**, 991 files in 710ms with no fixes.
3. `pnpm test` — **PASS**, 28/28 Turbo tasks in 11m11.808s. The compiler parallel suite passed
   220/220 files and 2,150/2,150 tests in 177.67s; `LocalSharedPressure.test.ts` passed 18/18 under
   repository contention, and the designated native differential corpus passed 1/1.
4. `pnpm check` — **PASS**. Biome checked 991 files in 722ms, the build completed 14/14 cached
   tasks, the combined typecheck/test phase completed 42/42 cached tasks, and all 16 repository
   script tests passed.
5. `pnpm release:candidate` — **PASS**. The build completed 14/14 cached tasks and release-candidate
   validation passed 1/1 file and 9/9 tests in 28.86s.

There were no gate retries or failures in this fresh sequence. The repair budget closed at 1/3.

## Scope and privilege dispositions

- Scheduler, Fiber, Deferred, Timer, Coroutine, reactor, queue, and cancellation-policy names stay
  confined to pressure fixtures and findings. They are not canonical APIs or compiler identities.
- The compiler recognizes only sealed Intrinsic Execution, Wake, and StorageFailure identities.
  The source-name inventory and renamed fixtures found no pressure-actor or allocation-policy
  spelling privilege.
- Shared capture alone retains `LocalExecution`; it does not select independent packages. A
  statically external-park-capable body keeps its external tier even when its executed branch is
  direct, while the owner-side driver remains NonParking.
- Concurrent Wake delivery, thread transfer, atomics, structured concurrency, implicit root
  ownership, fairness, and parallel scheduling remain the explicit SLP boundaries recorded in
  `findings.md`. This change adds no fallback or compatibility path for them.

## Task state and handoff

OpenSpec tasks remain **21/21 complete**. The parent-owned single three-lens conformance pass is
complete and all verified Critical/High findings are closed in its one consolidated fix pass. The
fresh gate-only resume closed the full-suite timeout and all required repository gates passed. No
second conformance pass was run.
