# Implementation report: prove-independent-execution-separation

Status: **READY FOR CONFORMANCE**

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

No hard-gate failure required the remaining repair slot.

## Focused verification

- `pnpm exec vitest run packages/compiler/test/ExternalWakeParking.test.ts packages/compiler/test/LocalSharedPressure.test.ts`
  — **PASS**, 2 files and 34 tests, 24.10s.
- `pnpm exec vitest run packages/compiler/test/DriverNativeAcceptance.test.ts` — **PASS**, 1 file
  and 1 differential-corpus test, 143.18s.
- Timer evaluation records the exact success and cancellation transition sequences. It also proves
  sibling progress before reactor polling, polling before endpoint notification, balanced
  allocation release, evaluator result `42`, and direct-Wasm result `42`.
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

## Scope and privilege dispositions

- Scheduler, Fiber, Deferred, Timer, Coroutine, reactor, queue, and cancellation-policy names stay
  confined to pressure fixtures and findings. They are not canonical APIs or compiler identities.
- The compiler recognizes only sealed Intrinsic Execution/Wake identities. The source-name
  inventory and renamed fixtures found no pressure-actor spelling privilege.
- Shared capture alone retains `LocalExecution`; it does not select independent packages. A
  statically external-park-capable body keeps its external tier even when its executed branch is
  direct, while the owner-side driver remains NonParking.
- Concurrent Wake delivery, thread transfer, atomics, structured concurrency, implicit root
  ownership, fairness, and parallel scheduling remain the explicit SLP boundaries recorded in
  `findings.md`. This change adds no fallback or compatibility path for them.

## Task state and handoff

OpenSpec tasks are **21/21 complete**. The work is ready for the parent-owned single three-lens
conformance pass. This worker did not start a conformance pass.
