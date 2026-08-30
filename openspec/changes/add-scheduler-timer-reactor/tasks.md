## 1. Establish the vertical timer contract

- [x] 1.1 Add one scripted-parent evaluator fixture in which a root forks a sleeping child and a
  ready sibling, then verify the sibling progresses before the child timer and the root joins the
  expected result in `packages/compiler/test/SchedulerFiber.test.ts`.
- [x] 1.2 Extend the scheduler fixture support with a deterministic parent `MonotonicClock` that
  records resolution reads, marks, and absolute waits, then verify fixtures can assert call order
  without ambient time or timing thresholds.
- [x] 1.3 Add compile-time fixtures for the new root and child requirement rows, then verify closed,
  Scheduler-only, MonotonicClock-using, and invalid extra-requirement child Effects receive the
  intended acceptance or diagnostic.

## 2. Build registration and timer state

- [x] 2.1 Add and document public pure `MonotonicClock.deadlineAfter`, use it from both OS and
  scheduler providers, then verify clock tests cover carry, negative origins, `u64.MAX`, and seconds
  overflow without duplicate arithmetic paths.
- [ ] 2.2 Add the private per-run shared timer-request inbox plus registration identity and
  `Idle`/`Armed` operations, then verify synchronous tests cover pending-inbox transfer, arming,
  claim, cancellation, stale generation, duplicate notification, and generation exhaustion.
- [ ] 2.3 Add the private stable timer min-heap keyed by canonical deadline and registration
  order, then verify synchronous tests cover empty, singleton, ascending and descending insertion,
  equal deadlines, checked order-counter exhaustion, root/middle/tail removal, and heap-index
  updates; keep the representation and operations private inside `silk.local_scheduler`.
- [ ] 2.4 Add heap-capacity reservation for root setup and atomic child adoption, then verify
  capacity covers `TaskStore.length + 1` before publication. Verify more children than the initial
  heap capacity can all publish before any parks, and allocation refusal publishes no child, arms
  no timer, and leaves heap contents/order unchanged even if unused reserved capacity remains.

## 3. Expand task environments and clock provision

- [x] 3.1 Update `Scheduler.prepare`, `Fiber.forkChild`, and their public documentation to accept
  child Effects requiring the owned Scheduler and MonotonicClock providers, then verify all
  scheduler and ownership fixtures typecheck without compatibility overloads or aliases.
- [x] 3.2 Add one private per-run shared timer inbox, clone its detached handle through every
  Scheduler client into each distinct owned task-clock client, and bind that clock with Scheduler
  inside each Execution wrapper. Verify nested work can wait without changing public
  `Scheduler.PreparedTask` or retaining a parent provider loan.
- [x] 3.3 Add per-run cached parent mark and resolution state; initialize it before root publication
  and refresh the mark at each turn, then verify task-local `now` does not park and repeated reads
  within one activation may remain equal.
- [x] 3.4 Implement task-clock `waitFor` and `waitUntil` registration, including immediate zero/past
  completion and non-parking allocation-free Wake installation, then verify future waits park once
  and relative waits request one fresh driver mark.
- [x] 3.5 Migrate every `LocalScheduler.execute` caller and fixture to provide an explicit parent
  MonotonicClock, then verify requirement closure reports the missing parent capability at an
  intentionally unresolved entry and no old entry shape remains.

## 4. Integrate timer progress into the driver

- [x] 4.1 Process the per-run timer inbox immediately after each selected task's `Execution.drive`,
  transfer the Wake out of Shared storage, and derive checked relative deadlines from one fresh
  parent-provider call. Verify the Wake/package cycle is absent before any trapping parent call;
  do not assert local cleanup after a fatal trap.
- [x] 4.2 Drain all timers due at the refreshed mark before selecting one ready task, consume their
  Wakes through existing readiness endpoints, and verify distinct deadlines and equal-deadline
  registration order append to the FIFO exactly once.
- [x] 4.3 Limit each scheduler turn to one selected task before refreshing time and collecting timers
  again. Use a scripted parent whose `now` advances on each driver refresh and verify a due timer
  fires before a known later yield while ready work is still continuously available.
- [x] 4.4 Replace empty-ready immediate stalling with private event collection: wait through the
  parent clock for the earliest timer, recheck the parent mark, and raise `StalledError` only when
  no active registration remains; verify future, past, spurious-equality, and no-registration cases.
- [ ] 4.5 Add a nested-scheduler fixture whose inner idle timer parks through the outer task-clock
  provider, then verify unrelated outer tasks progress and both scheduler runs cleanly resume. Add
  a separate bounded fixture documenting that a continuously-ready inner scheduler is a synchronous
  cooperative scope and does not promise fairness to outer siblings.

## 5. Make cancellation and failure cleanup exact

- [ ] 5.1 Extend normal timer completion to remove the heap interest and clear the matching armed
  registration before consuming its Wake, then verify duplicate and stale completion tokens are
  inert.
- [x] 5.2 Extend iterative descendant cancellation to disarm active timers, detach retained Wakes,
  destroy dormant Executions, and then drop the detached Wakes, verifying parent success, parent
  failure, and stalled shutdown leave no timer or package authority retained.
- [ ] 5.3 Implement the allocation matrix: root setup refusal raises `OutOfMemoryError` after typed
  cleanup; child reserve or TaskStore insertion refusal rejects only that publication and the run
  continues; post-park arming is pre-funded and any impossible append refusal traps as an invariant.
  Verify the exact outcome and cleanup boundary for each case.
- [x] 5.4 Extend root completion and provider-reuse fixtures with active far-future timers, then
  verify teardown makes first-run source indexes unreachable and a second `execute` observes fresh
  per-run registration state, cached clock state, ready state, and timer state.

## 6. Prove semantics at the cheapest tiers

- [x] 6.1 Audit the canonical fixtures and assertions established by tasks 1–5 for cached reads,
  zero and past waits, relative waits, sibling progress, ordering, yield fairness, cancellation,
  stalling, nested schedulers, and reuse; add only missing evaluator cases, share one Analysis
  snapshot per source program, and verify `SchedulerFiber.test.ts` passes.
- [ ] 6.2 Add allocation-ordinal sweeps only for root timer setup, child timer preparation, heap
  capacity reservation, and publication integration, then verify evaluator and Wasm run at every
  ordinal while native runs only at required boundary points.
- [x] 6.3 Add a pure scripted-clock timer program to direct-Wasm coverage and the shared native
  differential corpus, then verify all engines agree without adding a Wasm clock import or a
  feature-local native compilation loop.
- [x] 6.4 Add one shared-corpus case whose deterministic `source` uses a scripted parent and whose
  `nativeSource` variant provides `OsMonotonicClock` outside `execute`, recording sibling-before-
  timer order without elapsed-time, byte-count, or instruction-count assertions.
- [x] 6.5 Preserve direct OS-provider blocking coverage separately, then verify the existing clock
  ABI, interruption, and reachable-only tests still pass without claiming direct waits are
  fiber-aware.

## 7. Publish the breaking standard-library contract

- [x] 7.1 Update canonical doc comments and language-reference pages for LocalScheduler, Scheduler,
  Fiber, MonotonicClock, scheduler timers, stalled detection, cached marks, and direct OS blocking;
  verify generated documentation links and declaration signatures are current.
- [x] 7.2 Regenerate the standard-library manifest and any generated navigation or embedding output,
  then verify the repository contains no stale generated artifact or superseded scheduler entry
  signature.
- [x] 7.3 Run `pnpm typecheck`, `pnpm exec biome check .`, and `pnpm test` in that order, fix only
  failures caused by this change, and record any exact pre-existing failure.
- [x] 7.4 Run `pnpm check` and `pnpm release:candidate`, then verify the complete repository and
  changed package contents satisfy handoff requirements.
